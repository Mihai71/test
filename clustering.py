# clustering.py - Modul ML pentru detectarea bias-ului prin clustering nesupervizat
import pandas as pd
import numpy as np
import math
import json
from collections import defaultdict


def _load_df(file_path):
    fp = str(file_path).strip()
    if fp.endswith(('.xlsx', '.xls')):
        return pd.read_excel(fp)
    return pd.read_csv(fp)


def _normalize_series(s):
    s = pd.to_numeric(s, errors='coerce')
    mn, mx = s.min(), s.max()
    if mx == mn:
        return pd.Series(np.zeros(len(s)), index=s.index)
    return (s - mn) / (mx - mn)


# Mapare ordinală educație (română + engleză) — păstrează ierarhia în K-Means
_EDU_ORDINAL = {
    'fara scoala': 0, 'fara': 0, 'no education': 0,
    'primar': 1, 'elementar': 1, 'primary': 1, '4 clase': 1,
    'gimnaziu': 2, 'general': 2, 'scoala generala': 2, 'lower secondary': 2, '8 clase': 2,
    'liceu': 3, 'bacalaureat': 3, 'liceal': 3, 'high school': 3, 'upper secondary': 3, 'bac': 3,
    'postliceal': 4, 'profesional': 4, 'vocational': 4, 'colegiu': 4, 'post-secondary': 4,
    'facultate': 5, 'licenta': 5, 'bachelor': 5, 'universitar': 5, 'university': 5,
    'master': 6, 'masterat': 6, 'magistru': 6, 'postgraduate': 6, 'mba': 6,
    'doctorat': 7, 'phd': 7, 'doctor': 7, 'doctorate': 7,
}

# Cuvinte cheie sex din text
_FEMALE_KW = {'f', 'female', 'femeie', 'feminin', 'fem', 'w', 'woman', 'women'}
_MALE_KW   = {'m', 'male', 'barbat', 'masculin', 'man', 'men', 'b'}


def _encode_column(series, role=None):
    """
    role='edu'   → mapare ordinală (_EDU_ORDINAL) înainte de one-hot
    Numeric      → min-max normalizare
    Categoric ≤15 → one-hot (țări, rural/urban etc.)
    Categoric >15 → label encode + normalizare
    """
    s = series.copy()

    if role == 'edu':
        s_lower = s.astype(str).str.lower().str.strip()
        def _match_edu(x):
            for k, v in _EDU_ORDINAL.items():
                if k in x:
                    return v
            return None
        mapped = s_lower.map(_match_edu)
        if mapped.notna().mean() > 0.5:
            return pd.DataFrame({'_' + series.name: _normalize_series(mapped.astype(float))})

    s_num = pd.to_numeric(s, errors='coerce')
    if s_num.notna().mean() > 0.8:
        return pd.DataFrame({'_' + series.name: _normalize_series(s_num)})

    s_str = s.astype(str).str.strip().str.upper()
    unique_vals = [v for v in s_str.unique() if v not in ('NAN', 'NONE', '')]

    if len(unique_vals) <= 15:
        dummies = pd.get_dummies(s_str, prefix=series.name, drop_first=False)
        cols_ok = [c for c in dummies.columns if not c.endswith('_NAN')]
        return dummies[cols_ok].astype(float)
    else:
        mapping = {v: i for i, v in enumerate(sorted(unique_vals))}
        encoded = s_str.map(mapping)
        return pd.DataFrame({'_' + series.name: _normalize_series(encoded)})


# ---------------------------------------------------------------------------
# K-Means vectorizat cu NumPy
# ---------------------------------------------------------------------------

def _kmeans_numpy(X, k, max_iter=300, random_state=42):
    rng = np.random.RandomState(random_state)
    n, d = X.shape

    centroids = X[[rng.randint(0, n)]].copy()
    for _ in range(k - 1):
        diff     = X[:, np.newaxis, :] - centroids[np.newaxis, :, :]
        min_dist = (diff ** 2).sum(axis=2).min(axis=1)
        probs    = min_dist / min_dist.sum()
        centroids = np.vstack([centroids, X[[rng.choice(n, p=probs)]]])

    labels = np.zeros(n, dtype=int)
    for _ in range(max_iter):
        diff       = X[:, np.newaxis, :] - centroids[np.newaxis, :, :]
        new_labels = np.argmin((diff ** 2).sum(axis=2), axis=1)
        if np.all(new_labels == labels):
            break
        labels = new_labels
        for j in range(k):
            mask = labels == j
            if mask.sum() > 0:
                centroids[j] = X[mask].mean(axis=0)

    inertia = float(((X - centroids[labels]) ** 2).sum())
    return labels, centroids, inertia


def _pca_numpy(X, n_components=2, feature_labels=None):
    X_c = X - X.mean(axis=0)
    cov = np.cov(X_c.T)
    if cov.ndim == 0:
        cov = np.array([[float(cov)]])
    evals, evecs = np.linalg.eigh(cov)
    sort_idx  = np.argsort(evals)[::-1]
    evals     = evals[sort_idx]
    evecs     = evecs[:, sort_idx]
    components = evecs[:, :n_components]
    projected  = X_c.dot(components)

    total_var = evals.sum() if evals.sum() > 0 else 1.0
    expl_pct  = [round(float(evals[i] / total_var * 100), 1) for i in range(n_components)]

    pc_labels = []
    for i in range(n_components):
        loadings = np.abs(components[:, i])
        if feature_labels and len(feature_labels) == X.shape[1]:
            grouped = defaultdict(float)
            for j, name in enumerate(feature_labels):
                grouped[name] += float(loadings[j])
            top2      = sorted(grouped.items(), key=lambda x: x[1], reverse=True)[:2]
            top_names = [t[0] for t in top2]
        else:
            top_names = [f"F{j+1}" for j in np.argsort(loadings)[::-1][:2]]
        pc_labels.append(f"PC{i+1} ({expl_pct[i]}%): {' + '.join(top_names)}")

    return projected, pc_labels


# ---------------------------------------------------------------------------
# Elbow method
# ---------------------------------------------------------------------------

def compute_elbow(file_path, col_sex, col_age, col_edu, col_env,
                  col_income, col_extra_json="[]"):
    try:
        col_extra = json.loads(col_extra_json) if col_extra_json else []
        df = _load_df(file_path)

        frames = []
        col_role_pairs = [
            (str(col_sex), 'sex'), (str(col_age), 'age'), (str(col_edu), 'edu'),
            (str(col_env), 'env'), (str(col_income), 'income')
        ]
        for col, role in col_role_pairs:
            frames.append(_encode_column(df[col].copy(), role=role))
        for col in col_extra:
            if col in df.columns:
                frames.append(_encode_column(df[col].copy()))

        X_full  = pd.concat(frames, axis=1)
        X_clean = X_full[X_full.notna().all(axis=1)].values.astype(float)

        if len(X_clean) > 5000:
            idx      = np.random.RandomState(42).choice(len(X_clean), 5000, replace=False)
            X_sample = X_clean[idx]
        else:
            X_sample = X_clean

        results = []
        for k in range(2, 9):
            _, _, inertia = _kmeans_numpy(X_sample, k=k, max_iter=100)
            results.append({"k": k, "inertia": round(inertia, 2)})

        inertias    = np.array([r["inertia"] for r in results])
        diffs       = np.diff(inertias)
        suggested_k = int(np.argmax(np.abs(np.diff(diffs))) + 3)

        return {"error": None, "elbow_data": results, "suggested_k": suggested_k}
    except Exception as e:
        return {"error": str(e)}


# ---------------------------------------------------------------------------
# Auto-etichetare cluster
# ---------------------------------------------------------------------------

def _auto_label(profile, global_income_mean, global_age_mean):
    parts = []
    if profile.get('income_mean') is not None:
        inc = float(profile['income_mean'])
        if   inc < global_income_mean * 0.80: parts.append("Venit mic")
        elif inc > global_income_mean * 1.20: parts.append("Venit mare")
        else:                                  parts.append("Venit mediu")
    if profile.get('age_mean') is not None:
        age = float(profile['age_mean'])
        if   age < 35: parts.append("Tineri")
        elif age < 55: parts.append("Adulți")
        else:          parts.append("Seniori")
    if profile.get('female_pct') is not None:
        fp = float(profile['female_pct'])
        if   fp >= 65: parts.append("Majoritar F")
        elif fp <= 35: parts.append("Majoritar M")
    if profile.get('edu_mean') is not None:
        edu = float(profile['edu_mean'])
        if   edu <= 2.5: parts.append("Edu. scăzută")
        elif edu >= 5.5: parts.append("Edu. înaltă")
    return ", ".join(parts) if parts else f"Cluster {profile['cluster_id']}"


# ---------------------------------------------------------------------------
# Profilare clustere — FIX sex text + educatie text
# ---------------------------------------------------------------------------

def _profile_clusters(df, col_roles):
    profiles   = []
    col_sex    = col_roles['sex']
    col_age    = col_roles['age']
    col_edu    = col_roles['edu']
    col_env    = col_roles['env']
    col_income = col_roles['income']

    # Determină global perechea binară de sex (ignoră coduri speciale ca 9, 77, 99)
    all_sex_num      = pd.to_numeric(df[col_sex], errors='coerce').dropna()
    global_sex_top2  = sorted(all_sex_num.value_counts().nlargest(2).index.tolist())
    has_global_binary = len(global_sex_top2) >= 2

    for cid in sorted(df['_cluster'].unique()):
        grp = df[df['_cluster'] == cid]
        p   = {"cluster_id": int(cid), "n": len(grp),
               "pct": round(len(grp) / len(df) * 100, 1)}

        # Vârstă
        age_num = pd.to_numeric(grp[col_age], errors='coerce').dropna()
        if len(age_num) > 0:
            p["age_mean"]   = round(float(age_num.mean()), 1)
            p["age_median"] = round(float(age_num.median()), 1)

        # Sex — text (M/F) SAU numeric (1/2/ESS cu coduri speciale)
        sex_str    = grp[col_sex].astype(str).str.lower().str.strip()
        unique_sex = set(sex_str.unique()) - {'nan', 'none', ''}
        f_matches  = unique_sex & _FEMALE_KW
        m_matches  = unique_sex & _MALE_KW

        if f_matches or m_matches:
            all_known = f_matches | m_matches
            valid_sex = sex_str[sex_str.isin(all_known)]
            if len(valid_sex) > 0:
                f_count = int(valid_sex.isin(f_matches).sum())
                p["female_pct"] = round(f_count / len(valid_sex) * 100, 1)
                p["male_pct"]   = round(100 - p["female_pct"], 1)
        elif has_global_binary:
            # Numeric: folosește perechea globală (ex: 1=M, 2=F) ignorând 9/77/99
            sex_num    = pd.to_numeric(grp[col_sex], errors='coerce')
            v1, v2     = global_sex_top2[0], global_sex_top2[1]
            binary_sex = sex_num[sex_num.isin([v1, v2])]
            if len(binary_sex) > 0:
                f_count = int((binary_sex == v2).sum())
                p["female_pct"] = round(float(f_count / len(binary_sex) * 100), 1)
                p["male_pct"]   = round(100 - p["female_pct"], 1)

        # Educație
        edu_num = pd.to_numeric(grp[col_edu], errors='coerce')
        if edu_num.notna().mean() > 0.5:
            edu_vals        = edu_num.dropna()
            p["edu_mean"]   = round(float(edu_vals.mean()), 2)
            p["edu_is_text"] = False
        else:
            edu_text         = grp[col_edu].dropna().astype(str)
            p["edu_is_text"] = True
            if len(edu_text) > 0:
                p["edu_mode_text"] = str(edu_text.value_counts().index[0])
            mapped = edu_text.str.lower().str.strip().map(
                lambda x: next((v for k, v in _EDU_ORDINAL.items() if k in x), None)
            )
            if mapped.notna().mean() > 0.3:
                p["edu_mean"] = round(float(mapped.dropna().astype(float).mean()), 2)

        # Mediu/Origine
        env_vals = grp[col_env].dropna()
        env_num  = pd.to_numeric(env_vals, errors='coerce')
        if env_num.notna().mean() > 0.8:
            p["env_mean"] = round(float(env_num.mean()), 2)
        else:
            top = env_vals.astype(str).value_counts().head(3)
            p["env_top"] = {str(k): int(v) for k, v in top.items()}

        # Venit
        inc_num = pd.to_numeric(grp[col_income], errors='coerce').dropna()
        if len(inc_num) > 0:
            p["income_mean"]   = round(float(inc_num.mean()), 2)
            p["income_median"] = round(float(inc_num.median()), 2)
            p["income_min"]    = round(float(inc_num.min()), 2)
            p["income_max"]    = round(float(inc_num.max()), 2)
            if inc_num.nunique() <= 15:
                dist = inc_num.value_counts().sort_index()
                p["income_dist"] = {str(int(k)): int(v) for k, v in dist.items()}

        profiles.append(p)
    return profiles


# ---------------------------------------------------------------------------
# Analiză bias în interiorul clusterelor
# ---------------------------------------------------------------------------

def _analyze_bias_per_cluster(df, col_roles):
    col_sex    = col_roles['sex']
    col_age    = col_roles['age']
    col_env    = col_roles['env']
    col_income = col_roles['income']
    results    = []

    for cid in sorted(df['_cluster'].unique()):
        grp = df[df['_cluster'] == cid].copy()
        inc = pd.to_numeric(grp[col_income], errors='coerce')
        cb  = {"cluster_id": int(cid), "analyses": []}

        a = _bias_between_groups(grp, col_sex, inc, "Sex")
        if a: cb["analyses"].append(a)

        age_num = pd.to_numeric(grp[col_age], errors='coerce')
        if age_num.notna().sum() > 10:
            grp['_age_group'] = pd.cut(
                age_num, bins=[0, 35, 55, 150], labels=['<35', '35-55', '>55']
            ).astype(str)
            a = _bias_between_groups(grp, '_age_group', inc, "Vârstă")
            if a: cb["analyses"].append(a)

        a = _bias_between_groups(grp, col_env, inc, "Origine/Mediu")
        if a: cb["analyses"].append(a)

        if cb["analyses"]:
            max_effect       = max(a.get("cohen_d", 0) or 0 for a in cb["analyses"])
            bias_score       = round(min(max_effect * 0.7, 1.0), 4)
            cb["bias_score"] = bias_score
            cb["severity"]   = ("Neglijabil" if bias_score < 0.20
                                 else "Moderat" if bias_score < 0.50 else "Ridicat")
        results.append(cb)
    return results


def _bias_between_groups(df_grp, group_col, income_series, label):
    try:
        groups = {}
        for name, sub in df_grp.groupby(group_col):
            vals = income_series[sub.index].dropna().values
            if len(vals) >= 5:
                groups[str(name)] = vals
        if len(groups) < 2:
            return None

        group_means = {k: float(np.mean(v)) for k, v in groups.items()}
        result = {
            "attribute":   label,
            "groups":      list(groups.keys()),
            "means":       group_means,
            "n_per_group": {k: int(len(v)) for k, v in groups.items()}
        }

        if len(groups) == 2:
            g1, g2 = list(groups.values())
            n1, n2 = len(g1), len(g2)
            m1, m2 = np.mean(g1), np.mean(g2)
            s1 = float(np.std(g1, ddof=1)) if n1 > 1 else 0.0
            s2 = float(np.std(g2, ddof=1)) if n2 > 1 else 0.0
            pooled = math.sqrt(((n1-1)*s1**2 + (n2-1)*s2**2) / (n1+n2-2)) if (n1+n2-2) > 0 else 0.0
            d      = abs(m1 - m2) / pooled if pooled > 0 else 0.0
            result["cohen_d"]       = round(float(d), 4)
            result["mean_diff"]     = round(float(m1 - m2), 2)
            result["pct_diff"]      = round(float((m1-m2)/m2*100), 1) if m2 != 0 else None
            result["cohen_d_label"] = ("Neglijabil" if d < 0.2 else "Mic" if d < 0.5
                                        else "Mediu" if d < 0.8 else "Mare")
        else:
            means_arr = np.array(list(group_means.values()))
            result["cohen_d"]       = round(float(np.std(means_arr) / max(np.mean(means_arr), 0.001)), 4)
            result["cohen_d_label"] = "Multi-grup"

        return result
    except Exception:
        return None


# ---------------------------------------------------------------------------
# Funcție principală
# ---------------------------------------------------------------------------

def run_clustering(file_path, col_sex, col_age, col_edu, col_env,
                   col_income, col_extra_json="[]", n_clusters=4):
    try:
        n_clusters = int(n_clusters)
        col_extra  = json.loads(col_extra_json) if col_extra_json else []
        df         = _load_df(file_path)

        col_roles = {'sex': str(col_sex), 'age': str(col_age),
                     'edu': str(col_edu), 'env': str(col_env), 'income': str(col_income)}

        for c in col_roles.values():
            if c not in df.columns:
                return {"error": f"Coloana '{c}' nu există în fișier."}

        role_labels = {'sex': 'Sex', 'age': 'Vârstă', 'edu': 'Educație',
                       'env': 'Mediu/Origine', 'income': 'Venit'}
        col_to_role = {v: k for k, v in col_roles.items()}

        frames         = []
        feature_labels = []
        all_cols = list(col_roles.values()) + [c for c in col_extra if c in df.columns]

        for col in all_cols:
            role_key = col_to_role.get(col, '')
            encoded  = _encode_column(df[col].copy(), role=role_key)
            human    = role_labels.get(role_key, col)
            feature_labels.extend([human] * encoded.shape[1])
            frames.append(encoded)

        X_full     = pd.concat(frames, axis=1)
        valid_mask = X_full.notna().all(axis=1)
        X_clean    = X_full[valid_mask].values.astype(float)
        df_clean   = df[valid_mask].copy()

        if len(X_clean) < n_clusters * 2:
            return {"error": "Date insuficiente pentru clustering."}

        labels, _, inertia = _kmeans_numpy(X_clean, k=n_clusters)
        df_clean = df_clean.copy()
        df_clean['_cluster'] = labels

        pca_coords, pc_labels = _pca_numpy(X_clean, n_components=2,
                                           feature_labels=feature_labels)
        df_clean['_pca_x'] = pca_coords[:, 0]
        df_clean['_pca_y'] = pca_coords[:, 1]

        profiles     = _profile_clusters(df_clean, col_roles)
        bias_results = _analyze_bias_per_cluster(df_clean, col_roles)

        global_inc = float(pd.to_numeric(df_clean[col_income], errors='coerce').mean())
        global_age = float(pd.to_numeric(df_clean[col_age],    errors='coerce').mean())
        for p in profiles:
            p['label'] = _auto_label(p, global_inc, global_age)

        pca_out = df_clean[['_cluster', '_pca_x', '_pca_y']].copy()
        for col in col_roles.values():
            pca_out[col] = df_clean[col].values

        return {
            "error":            None,
            "n_rows_used":      int(len(X_clean)),
            "n_rows_total":     int(len(df)),
            "n_clusters":       n_clusters,
            "inertia":          round(inertia, 2),
            "pc_labels":        pc_labels,
            "profiles":         profiles,
            "bias_per_cluster": bias_results,
            "pca_data":         pca_out.to_dict(orient='list')
        }
    except Exception as e:
        return {"error": f"Eroare clustering: {str(e)}"}
