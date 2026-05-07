# logic.py
import pandas as pd
import re

def profile_data(file_path):
    df = pd.read_csv(file_path)
    
    # Verificare minim 2 coloane (Atribut Sensibil + Target)
    if df.shape[1] < 2:
        return {"error": "Fișierul trebuie să conțină cel puțin 2 coloane."}
    
    # Verificare dacă există rânduri de date după header
    if df.empty:
        return {"error": "Fișierul este gol sau conține doar header-ul."}
    
    # 1. Detecție valori lipsă detaliată (FR-05)
    missing_info = df.isnull().sum().to_dict()
    
    # 2. Identificare tipuri (FR-01)
    types_info = {}
    for col in df.columns:
        if df[col].dtype in ['int64', 'float64']:
            types_info[col] = "Numerică"
        elif df[col].nunique() == 2:
            types_info[col] = "Binară"
        else:
            types_info[col] = "Categorică"
            
    # 3. Detecție inteligentă atribute sensibile (Pattern Matching)
    sensitive_patterns = r"(gen|sex|gender|v[âa]rst[ăa]|age|educa|regiu|mediu|etnie)"
    detected_sensitive = [col for col in df.columns if re.search(sensitive_patterns, col.lower())]
    
    # 4. Detecție prioritară Target Financiar (Salariu/Venit)
    financial_patterns = r"(salariu|venit|pensie|income|wage|cheltuieli)"
    detected_financial = [col for col in df.columns if re.search(financial_patterns, col.lower())]
    
    return {
        "columns": list(df.columns),
        "missing": missing_info,
        "types": types_info,
        "sensitive_candidates": detected_sensitive,
        "financial_candidates": detected_financial
    }
