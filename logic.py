# logic.py - Backend Python pentru analiza disparitatilor (FR-01, FR-04, FR-05, FR-06)
import pandas as pd
import numpy as np
import re
import math


def _load_file(file_path):
    fp = str(file_path).strip()
    if fp.endswith(('.xlsx', '.xls')):
        return pd.read_excel(fp)
    return pd.read_csv(fp)


def profile_data(file_path):
    try:
        df = _load_file(file_path)
    except Exception as e:
        return {"error": "Eroare la citirea fisierului: " + str(e)}

    if df.shape[1] < 2:
        return {"error": "Fisierul trebuie sa contina cel putin 2 coloane (Atribut Sensibil + Target)."}
    if len(df) == 0:
        return {"error": "Fisierul este gol sau contine doar header-ul."}

    missing_info = {col: int(df[col].isnull().sum()) for col in df.columns}

    types_info = {}
    for col in df.columns:
        n_unique = int(df[col].nunique(dropna=True))
        is_numeric = pd.api.types.is_numeric_dtype(df[col])
        if n_unique == 2:
            types_info[col] = "Binara"
        elif is_numeric:
            types_info[col] = "Numerica"
        else:
            types_info[col] = "Categorica"

    sensitive_patterns = r"(gen|sex|gender|v[ae]rst[a]|age|educa[t]|studi|regiu|mediu|etnie|national|urban|rural|zona|judet|localit)"
    detected_sensitive = [c for c in df.columns if re.search(sensitive_patterns, c.lower())]

    financial_patterns = r"(salariu|venit|pensie|income|wage|cheltuiel|salary|earning|plata|remuner)"
    detected_financial = [c for c in df.columns if re.search(financial_patterns, c.lower())]

    return {
        "columns": list(df.columns),
        "missing": missing_info,
        "types": types_info,
        "sensitive_candidates": detected_sensitive,
        "financial_candidates": detected_financial,
        "n_rows": int(df.shape[0]),
        "n_cols": int(df.shape[1])
    }


def compute_distribution_alerts(file_path, col):
    try:
        df = _load_file(file_path)
    except Exception as e:
        return {"error": str(e)}

    series = pd.to_numeric(df[col], errors='coerce').dropna()
    if len(series) < 3:
        return {"skewness": None, "outliers_count": 0, "outliers_pct": 0.0, "n": 0}

    skewness = float(series.skew())
    q1 = float(series.quantile(0.25))
    q3 = float(series.quantile(0.75))
    iqr = q3 - q1
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    outliers = series[(series < lower) | (series > upper)]

    return {
        "skewness": round(skewness, 4),
        "outliers_count": int(len(outliers)),
        "outliers_pct": round(float(len(outliers) / len(series) * 100), 2),
        "n": int(len(series)),
        "q1": round(q1, 2),
        "q3": round(q3, 2),
        "lower_fence": round(float(lower), 2),
        "upper_fence": round(float(upper), 2)
    }


def compute_group_imbalance(file_path, col):
    try:
        df = _load_file(file_path)
    except Exception as e:
        return []

    counts = df[col].value_counts(normalize=True)
    alerts = []
    for group, pct in counts.items():
        if float(pct) < 0.20:
            alerts.append({"group": str(group), "pct": round(float(pct * 100), 2)})
    return alerts


def compute_numeric_metrics(file_path, sensitive_col, target_col):
    try:
        df = _load_file(file_path)
    except Exception as e:
        return {"error": str(e)}

    df[target_col] = pd.to_numeric(df[target_col], errors='coerce')
    df = df.dropna(subset=[target_col, sensitive_col])

    groups_series = [grp[target_col].values for _, grp in df.groupby(sensitive_col)]
    group_names = list(df.groupby(sensitive_col).groups.keys())
    n_groups = len(groups_series)

    summary = []
    for name, arr in zip(group_names, groups_series):
        summary.append({
            "Grup": str(name),
            "N": int(len(arr)),
            "Media": round(float(np.mean(arr)), 2),
            "Mediana": round(float(np.median(arr)), 2),
            "SD": round(float(np.std(arr, ddof=1)) if len(arr) > 1 else 0.0, 2)
        })

    result = {"summary": summary, "n_groups": n_groups}

    if n_groups >= 2:
        f_stat, p_anova = _one_way_anova(groups_series)
        result["f_stat"] = round(float(f_stat), 4)
        result["p_value_anova"] = round(float(p_anova), 6)

    if n_groups == 2:
        g1, g2 = groups_series
        n1, n2 = len(g1), len(g2)
        m1, m2 = float(np.mean(g1)), float(np.mean(g2))
        s1 = float(np.std(g1, ddof=1)) if n1 > 1 else 0.0
        s2 = float(np.std(g2, ddof=1)) if n2 > 1 else 0.0

        mean_diff = m1 - m2
        pct_diff = (mean_diff / m2 * 100) if m2 != 0 else None
        t_stat, p_ttest = _welch_ttest(g1, g2)
        s_pooled = math.sqrt(((n1-1)*s1**2 + (n2-1)*s2**2) / (n1+n2-2)) if (n1+n2-2) > 0 else 0.0
        cohen_d = abs(mean_diff) / s_pooled if s_pooled > 0 else 0.0

        result["mean_diff"] = round(float(mean_diff), 2)
        result["pct_diff"] = round(float(pct_diff), 2) if pct_diff is not None else None
        result["t_stat"] = round(float(t_stat), 4)
        result["p_value_ttest"] = round(float(p_ttest), 6)
        result["cohen_d"] = round(float(cohen_d), 4)

        if cohen_d < 0.2:   cohend_interp = "Neglijabil (< 0.2)"
        elif cohen_d < 0.5: cohend_interp = "Mic (0.2 - 0.49)"
        elif cohen_d < 0.8: cohend_interp = "Mediu (0.5 - 0.79)"
        else:               cohend_interp = "Mare (>= 0.8)"
        result["cohen_d_interpretation"] = cohend_interp

    return result


def compute_binary_metrics(file_path, sensitive_col, target_col):
    try:
        df = _load_file(file_path)
    except Exception as e:
        return {"error": str(e)}

    df = df.dropna(subset=[target_col, sensitive_col])
    vals = sorted(df[target_col].dropna().unique(), key=str)
    success_val = vals[-1]

    summary = []
    for name, grp in df.groupby(sensitive_col):
        total = len(grp)
        successes = int((grp[target_col] == success_val).sum())
        rate = successes / total if total > 0 else 0.0
        summary.append({
            "Grup": str(name), "Total": total,
            "Succese": successes, "Rata_Succes": round(float(rate), 4)
        })

    result = {"summary": summary, "success_label": str(success_val), "n_groups": len(summary)}

    if len(summary) == 2:
        p1 = summary[0]["Rata_Succes"]
        p2 = summary[1]["Rata_Succes"]
        spd = p1 - p2
        di  = (p1 / p2) if p2 != 0 else None

        result["spd"] = round(float(spd), 4)
        result["disparate_impact"] = round(float(di), 4) if di is not None else None
        result["risk_ratio"]       = round(float(di), 4) if di is not None else None

        if di is not None:
            if 0.8 <= di <= 1.25: di_interp = "Echitabil (regula 80%: DI intre 0.8-1.25)"
            elif di < 0.8:        di_interp = "Risc de discriminare (DI < 0.8)"
            else:                 di_interp = "Favorizare inversa (DI > 1.25)"
            result["di_interpretation"] = di_interp

    return result


def compute_bias_score(effect_size, group_proportions_list):
    effect_size = float(effect_size)
    props = [float(p) for p in group_proportions_list]
    effect_norm = min(effect_size, 1.0)
    imbalance_penalty = 0.0
    if props:
        min_prop = min(props)
        if min_prop < 0.20:
            imbalance_penalty = (0.20 - min_prop) / 0.20

    bias_score = round(min(0.7 * effect_norm + 0.3 * imbalance_penalty, 1.0), 4)

    if bias_score < 0.20:   severity, color, icon = "Neglijabil", "success", "check-circle"
    elif bias_score < 0.50: severity, color, icon = "Moderat",    "warning", "exclamation-triangle"
    else:                   severity, color, icon = "Ridicat",    "danger",  "times-circle"

    return {
        "bias_score": bias_score, "severity": severity, "color": color, "icon": icon,
        "effect_component": round(effect_norm, 4),
        "imbalance_component": round(imbalance_penalty, 4)
    }


def _welch_ttest(a, b):
    n1, n2 = len(a), len(b)
    if n1 < 2 or n2 < 2: return (float('nan'), float('nan'))
    m1, m2 = np.mean(a), np.mean(b)
    v1, v2 = np.var(a, ddof=1), np.var(b, ddof=1)
    se = math.sqrt(v1/n1 + v2/n2)
    if se == 0: return (float('nan'), float('nan'))
    t = (m1 - m2) / se
    df = (v1/n1 + v2/n2)**2 / ((v1/n1)**2/(n1-1) + (v2/n2)**2/(n2-1))
    return (float(t), float(_t_pvalue(abs(t), df)))


def _one_way_anova(groups):
    all_vals = np.concatenate(groups)
    grand_mean = np.mean(all_vals)
    k = len(groups)
    if k < 2: return (float('nan'), float('nan'))
    ss_between = sum(len(g) * (np.mean(g) - grand_mean)**2 for g in groups)
    ss_within  = sum(np.sum((g - np.mean(g))**2) for g in groups)
    df_b, df_w = k - 1, len(all_vals) - k
    if df_w <= 0 or ss_within == 0: return (float('nan'), float('nan'))
    f = (ss_between/df_b) / (ss_within/df_w)
    return (float(f), float(_f_pvalue(float(f), df_b, df_w)))


def _t_pvalue(t, df):
    try:
        x = df / (df + t*t)
        return float(2.0 * _regularized_incomplete_beta(df/2.0, 0.5, x) / 2.0)
    except: return float('nan')


def _f_pvalue(f, df1, df2):
    try:
        x = df2 / (df2 + df1*f)
        return float(_regularized_incomplete_beta(df2/2.0, df1/2.0, x))
    except: return float('nan')


def _regularized_incomplete_beta(a, b, x, max_iter=200, tol=1e-10):
    if x < 0 or x > 1: return float('nan')
    if x == 0: return 0.0
    if x == 1: return 1.0
    if x > (a+1)/(a+b+2):
        return 1.0 - _regularized_incomplete_beta(b, a, 1.0-x, max_iter, tol)
    lbeta = math.lgamma(a+b) - math.lgamma(a) - math.lgamma(b)
    front = math.exp(lbeta + a*math.log(x) + b*math.log(1.0-x)) / a
    f = c = 1.0
    d = 1.0 - (a+b)*x/(a+1)
    if abs(d) < 1e-30: d = 1e-30
    d = 1.0/d; f = d
    for m in range(1, max_iter+1):
        for num in [m*(b-m)*x/((a+2*m-1)*(a+2*m)),
                    -(a+m)*(a+b+m)*x/((a+2*m)*(a+2*m+1))]:
            d = 1.0 + num*d; c = 1.0 + num/c
            if abs(d) < 1e-30: d = 1e-30
            if abs(c) < 1e-30: c = 1e-30
            d = 1.0/d; delta = c*d; f *= delta
        if abs(delta - 1.0) < tol: break
    return float(front * f)
