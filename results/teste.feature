Feature: Testare completă API pentru Detectarea Disparităților Socio-Economice

  Background:
    * url 'http://127.0.0.1:8000'
    * def setup = callonce read('setup.feature')
    * def fileId = setup.fId

# ============================================================
# GRUP 1 — FILE MANAGEMENT
# ============================================================

  Scenario: G1-01 Upload fisier CSV valid si retinere file_id
    Given path '/upload'
    And multipart file file = { read: 'test_data.csv', filename: 'test_data.csv', contentType: 'text/csv' }
    When method POST
    Then status 200
    And match response.file_id[0] == '#string'
    And match response.filename[0] == 'test_data.csv'
    And match response.rows[0] == 25
    And match response.cols[0] == 8
    And match response.col_names == '#notnull'

  Scenario: G1-02 Previzualizare primele 5 randuri (implicit)
    Given path '/files/' + fileId + '/preview'
    When method GET
    Then status 200
    And match response.rows_shown[0] == 5
    And match response.rows_total[0] == 25
    And match response.data == '#array'

  Scenario: G1-03 Previzualizare cu N specificat (10 randuri)
    Given path '/files/' + fileId + '/preview'
    And param n = 10
    When method GET
    Then status 200
    And match response.rows_shown[0] == 10

  Scenario: G1-04 Upload format nesuportat returneaza 415
    Given path '/upload'
    And multipart file file = { value: 'continut test', filename: 'test.txt', contentType: 'text/plain' }
    When method POST
    Then status 415
    And match response.error == '#notnull'

  Scenario: G1-05 Preview pentru file_id inexistent returneaza 404
    Given path '/files/idnuexista9999/preview'
    When method GET
    Then status 404
    And match response.error == '#notnull'

# ============================================================
# GRUP 2 — DATA PROFILING
# ============================================================

  Scenario: G2-01 Profil complet al fisierului — toate coloanele
    Given path '/profile'
    And param file_id = fileId
    When method POST
    Then status 200
    And match response.rows[0] == 25
    And match response.cols[0] == 8
    And match response.columns == '#array'
    And match response.columns[0].name == '#notnull'
    And match response.columns[0].detected_type == '#notnull'
    And match response.columns[0].missing_pct == '#notnull'

  Scenario: G2-02 Validare coloane corecte — target numeric
    Given path '/validate'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.valid[0] == true
    And match response.sensitive_type[0] == 'binary'
    And match response.target_type[0] == 'numeric'
    And match response.n_sens_groups[0] == 2

  Scenario: G2-03 Validare coloane corecte — target binar
    Given path '/validate'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    When method POST
    Then status 200
    And match response.valid[0] == true
    And match response.target_type[0] == 'binary'

  Scenario: G2-04 Validare atribut cu 3+ grupuri — nota informativa
    Given path '/validate'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.valid[0] == true
    And match response.n_sens_groups[0] == 3
    And match response.note == '#notnull'

  Scenario: G2-05 Validare coloane identice returneaza eroare
    Given path '/validate'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'sex'
    When method POST
    Then status 400
    And match response.valid[0] == false

# ============================================================
# GRUP 3 — METRICI NUMERICE
# ============================================================

  Scenario: G3-01 Statistici descriptive per grup (sex → salariu)
    Given path '/metrics/descriptive'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.n_groups[0] == 2
    And match response.groups == '#array'
    And match response.groups[0].group == '#notnull'
    And match response.groups[0].count == '#notnull'
    And match response.groups[0].mean == '#notnull'

  Scenario: G3-02 Statistici descriptive 3+ grupuri (educatie → salariu)
    Given path '/metrics/descriptive'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.n_groups[0] == 3

  Scenario: G3-03 Diferenta mediilor (sex → salariu)
    Given path '/metrics/mean-diff'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.group1 == '#notnull'
    And match response.group2 == '#notnull'
    And match response.mean1 == '#notnull'
    And match response.mean2 == '#notnull'
    And match response.abs_diff == '#notnull'
    And match response.pct_diff == '#notnull'

  Scenario: G3-04 Diferenta mediilor cu group1 si group2 specificati (educatie)
    Given path '/metrics/mean-diff'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    And param group1 = 'Facultate'
    And param group2 = 'Scoala'
    When method POST
    Then status 200
    And match response.group1[0] == 'Facultate'
    And match response.group2[0] == 'Scoala'
    And match response.abs_diff == '#notnull'

  Scenario: G3-05 Cohens d (sex → salariu) — marimi de efect
    Given path '/metrics/cohens-d'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.cohens_d == '#notnull'
    And match response.cohens_d_abs == '#notnull'
    And match response.magnitude == '#notnull'
    And match response.sd_pooled == '#notnull'
    And match response.thresholds == '#notnull'

  Scenario: G3-06 Cohens d cu 3 grupuri si pereche specificata (Facultate vs Scoala)
    Given path '/metrics/cohens-d'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    And param group1 = 'Facultate'
    And param group2 = 'Scoala'
    When method POST
    Then status 200
    And match response.cohens_d == '#notnull'
    And match response.group1[0] == 'Facultate'
    And match response.group2[0] == 'Scoala'

  Scenario: G3-07 Welch t-test (sex → salariu)
    Given path '/metrics/welch-ttest'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.t_statistic == '#notnull'
    And match response.p_value == '#notnull'
    And match response.degrees_of_freedom == '#notnull'
    And match response.significant == '#notnull'
    And match response.alpha == '#notnull'
    And match response.confidence_interval == '#notnull'

  Scenario: G3-08 ANOVA Welch (educatie → salariu, 3 grupuri)
    Given path '/metrics/anova'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.n_groups[0] == 3
    And match response.f_statistic == '#notnull'
    And match response.p_value == '#notnull'
    And match response.significant == '#notnull'
    And match response.method == '#notnull'

  Scenario: G3-09 Cohens d cu group1 invalid returneaza eroare
    Given path '/metrics/cohens-d'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    And param group1 = 'GrupNuExista'
    And param group2 = 'Scoala'
    When method POST
    Then status 400
    And match response.error == '#notnull'

# ============================================================
# GRUP 4 — METRICI BINARE
# ============================================================

  Scenario: G4-01 Statistical Parity Difference (sex → angajat)
    Given path '/metrics/spd'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match response.positive_value == '#notnull'
    And match response.group_privileged == '#notnull'
    And match response.group_protected == '#notnull'
    And match response.spd == '#notnull'
    And match response.equitable == '#notnull'
    And match response.interpretation == '#notnull'

  Scenario: G4-02 Disparate Impact (sex → angajat) — regula 80%
    Given path '/metrics/disparate-impact'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match response.disparate_impact == '#notnull'
    And match response.equitable == '#notnull'
    And match response.rule_80_pct == '#notnull'

  Scenario: G4-03 SPD cu 3 grupuri — group1 si group2 obligatorii (educatie → angajat)
    Given path '/metrics/spd'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    And param group1 = 'Facultate'
    And param group2 = 'Scoala'
    When method POST
    Then status 200
    And match response.spd == '#notnull'

  Scenario: G4-04 SPD cu 3 grupuri fara group1/group2 returneaza eroare
    Given path '/metrics/spd'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'angajat'
    When method POST
    Then status 400
    And match response.error == '#notnull'

# ============================================================
# GRUP 5 — ALERTE DE CALITATE DATE
# ============================================================

  Scenario: G5-01 Analiza distributiei target numeric — skewness si outlieri
    Given path '/alerts/distribution'
    And param file_id = fileId
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.n_values[0] == 25
    And match response.skewness == '#notnull'
    And match response.skew_level == '#notnull'
    And match response.q1 == '#notnull'
    And match response.q3 == '#notnull'
    And match response.iqr == '#notnull'
    And match response.outlier_count == '#notnull'
    And match response.outlier_severity == '#notnull'

  Scenario: G5-02 Dezechilibru grupuri atribut sensibil (sex — echilibrat)
    Given path '/alerts/imbalance'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    When method POST
    Then status 200
    And match response.n_groups[0] == 2
    And match response.imbalanced[0] == false
    And match response.severity[0] == 'ok'

  Scenario: G5-03 Dezechilibru grupuri atribut cu 3 categorii (educatie)
    Given path '/alerts/imbalance'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    When method POST
    Then status 200
    And match response.n_groups[0] == 3
    And match response.groups == '#array'

  Scenario: G5-04 Raport valori lipsa per coloana
    Given path '/alerts/missing/' + fileId
    When method GET
    Then status 200
    And match response.n_rows[0] == 25
    And match response.n_cols[0] == 8
    And match response.columns == '#array'
    And match response.has_missing[0] == false

  Scenario: G5-05 Alerta distributie pentru coloana non-numerica returneaza 400
    Given path '/alerts/distribution'
    And param file_id = fileId
    And param target_col = 'sex'
    When method POST
    Then status 400

# ============================================================
# GRUP 6 — BIAS SCORE
# ============================================================

  Scenario: G6-01 Bias score target numeric — 2 grupuri (sex → salariu)
    Given path '/bias-score'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.target_type[0] == 'numeric'
    And match response.effect_metric[0] == "Cohen's d"
    And match response.bias_score == '#notnull'
    And match response.severity == '#notnull'
    And match response.formula == '#notnull'

  Scenario: G6-02 Bias score target binar — 2 grupuri (sex → angajat)
    Given path '/bias-score'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match response.effect_metric[0] == 'SPD'
    And match response.bias_score == '#notnull'

  Scenario: G6-03 Bias score 3+ grupuri fara pairwise — foloseste eta-squared
    Given path '/bias-score'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.effect_metric == '#notnull'
    And match response.bias_score == '#notnull'

  Scenario: G6-04 Bias score 3+ grupuri cu pairwise explicit — foloseste Cohens d
    Given path '/bias-score'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    And param group1 = 'Facultate'
    And param group2 = 'Scoala'
    When method POST
    Then status 200
    And match response.effect_metric[0] == "Cohen's d"

# ============================================================
# GRUP 7 — SOCIO-DEMOGRAFIC
# ============================================================

  Scenario: G7-01 Date de referinta Eurostat — structura completa
    Given path '/socio/reference'
    When method GET
    Then status 200
    And match response.reference_year[0] == 2022
    And match response.source == '#notnull'
    And match response.indicators.pay_gap.values.RO[0] == 3.6
    And match response.indicators.pay_gap.values.EU[0] == 12.7
    And match response.indicators.employment_rate.values.RO == '#notnull'
    And match response.indicators.tertiary_education.values.RO == '#notnull'
    And match response.ro_region_population_pct == '#notnull'

  Scenario: G7-02 Distributie pe grupe de varsta
    Given path '/socio/age'
    And param file_id = fileId
    And param age_col = 'varsta'
    When method POST
    Then status 200
    And match response.age_groups == '#array'
    And match response.age_groups[0].group == '#notnull'
    And match response.age_groups[0].count == '#notnull'
    And match response.mean_age == '#notnull'
    And match response.median_age == '#notnull'
    And match response.reference == '#notnull'

  Scenario: G7-03 Distributie nivel educatie cu comparatie Eurostat
    Given path '/socio/education'
    And param file_id = fileId
    And param education_col = 'educatie'
    When method POST
    Then status 200
    And match response.n_levels[0] == 3
    And match response.levels == '#array'
    And match response.pct_tertiary_est == '#notnull'
    And match response.reference == '#notnull'

  Scenario: G7-04 Distributie regionala cu comparatie INS Romania
    Given path '/socio/region'
    And param file_id = fileId
    And param region_col = 'regiune'
    When method POST
    Then status 200
    And match response.regions == '#array'
    And match response.reference_source == '#notnull'

  Scenario: G7-05 Analiza varsta pe coloana non-numerica returneaza 400
    Given path '/socio/age'
    And param file_id = fileId
    And param age_col = 'sex'
    When method POST
    Then status 400

# ============================================================
# GRUP 8 — ANALIZA COMPLETA
# ============================================================

  Scenario: G8-01 Analiza completa target numeric (sex → salariu)
    Given path '/analyze'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.target_type == '#notnull'
    And match response.n_sens_groups == '#notnull'
    And match response.pairwise_mode == '#notnull'
    And match response.quality == '#notnull'
    And match response.metrics_numeric == '#notnull'
    And match response.bias_score == '#notnull'

  Scenario: G8-02 Analiza completa target binar (sex → angajat)
    Given path '/analyze'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match response.target_type == '#notnull'
    And match response.metrics_binary == '#notnull'
    And match response.bias_score == '#notnull'

  Scenario: G8-03 Analiza completa cu 3+ grupuri si pereche explicita (educatie → salariu)
    Given path '/analyze'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    And param group1 = 'Facultate'
    And param group2 = 'Scoala'
    When method POST
    Then status 200
    And match response.n_sens_groups == '#notnull'
    And match response.pairwise_mode == '#notnull'
    And match response.metrics_numeric == '#notnull'

  Scenario: G8-04 Analiza completa cu 3+ grupuri fara pereche — nota in raspuns
    Given path '/analyze'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.pairwise_mode == '#notnull'
    And match response.metrics_numeric == '#notnull'

# ============================================================
# GRUP 9 — VIZUALIZARI
# ============================================================

  Scenario: G9-01 Boxplot target numeric (sex → salariu) — PNG base64
    Given path '/viz/boxplot'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.format[0] == 'png'
    And match response.image_base64 == '#notnull'
    And match response.n_groups[0] == 2

  Scenario: G9-02 Density plot target numeric (educatie → salariu, 3 grupuri)
    Given path '/viz/density'
    And param file_id = fileId
    And param sensitive_col = 'educatie'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match response.image_base64 == '#notnull'
    And match response.n_groups[0] == 3

  Scenario: G9-03 Bar chart proportii target binar (sex → angajat)
    Given path '/viz/barplot'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match response.positive_value == '#notnull'
    And match response.image_base64 == '#notnull'

  Scenario: G9-04 Grafic paritate SPD si DI (sex → angajat)
    Given path '/viz/parity'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match response.spd == '#notnull'
    And match response.di == '#notnull'
    And match response.group_privileged == '#notnull'
    And match response.group_protected == '#notnull'
    And match response.image_base64 == '#notnull'

# ============================================================
# GRUP 10 — EXPORT
# ============================================================

  Scenario: G10-01 Export raport JSON descarcabil
    Given path '/export/report'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match responseHeaders['Content-Disposition'][0] contains 'attachment'

  Scenario: G10-02 Export grafice arhiva ZIP (target numeric)
    Given path '/export/charts'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match responseHeaders['Content-Disposition'][0] contains '.zip'

  Scenario: G10-03 Export grafice arhiva ZIP (target binar)
    Given path '/export/charts'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'angajat'
    And param positive_value = 'da'
    When method POST
    Then status 200
    And match responseHeaders['Content-Disposition'][0] contains '.zip'

  Scenario: G10-04 Export complet ZIP — raport JSON plus grafice
    Given path '/export/full'
    And param file_id = fileId
    And param sensitive_col = 'sex'
    And param target_col = 'salariu'
    When method POST
    Then status 200
    And match responseHeaders['Content-Disposition'][0] contains '.zip'

# ============================================================
# GRUP 1 — STERGERE (la final, dupa toate testele)
# ============================================================

  Scenario: G1-06 Stergere fisier din memorie
    Given path '/files/' + fileId
    When method DELETE
    Then status 200
    And match response.success[0] == true
    And match response.message == '#notnull'

  Scenario: G1-07 Stergere a doua oara returneaza 404
    Given path '/files/' + fileId
    When method DELETE
    Then status 404