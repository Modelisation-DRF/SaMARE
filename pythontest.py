
# python_code.py
import pandas as pd

def manipulate_dataframe(df):
    
    df = df.copy()
    
    for i in range(10000):  # Boucle de 20 fois
        # Exemple de calcul: doubler les valeurs de la première colonne
        # Remarquez comment nous utilisons i pour créer un nom de colonne unique à chaque itération
        df[f'calc_{i}'] = df[df.columns[0]] * 2
    
    return df
