# Tarification en Assurance Auto — Modélisation Linéaire Généralisée (GLM)

## Aperçu
Ce projet présente une approche actuarielle de la tarification en assurance automobile, s’appuyant sur des **modèles linéaires généralisés (GLM)** pour modéliser la **fréquence** et la **sévérité des sinistres**.

Il couvre l’ensemble du processus analytique, incluant :
- Analyse exploratoire des données
- Sélection et transformation des variables
- Développement des modèles (fréquence et sévérité)
- Validation et diagnostic des modèles
- Interprétation des résultats alignée avec les pratiques en assurance IARD

L’objectif est de reproduire un **cadre réaliste de tarification actuarielle** et de démontrer comment la modélisation statistique soutient des **décisions de tarification fondées sur les données**.

---

## Méthodologie

### Définitions – Cadre méthodologique

Les indicateurs suivants sont utilisés selon les conventions actuarielles usuelles :

- **Exposition (∑Ei)** : volume d’assurés constituant la base du risque  
- **Sinistres (∑Ni)** : nombre total d’événements déclarés  
- **Coût des sinistres (∑Si)** : montant agrégé des paiements  

#### Fréquence

Freq = (∑Ni) / (∑Ei)

Mesure de l’intensité d’occurrence des sinistres.

#### Sévérité

Sev = (∑Si) / (∑Ni)

Coût moyen par sinistre.

#### Prime pure

PP = Freq × Sev = (∑Si) / (∑Ei)

Indicateur du coût technique attendu par unité d’exposition.

Les montants sont exprimés en **dollars canadiens (CAD)**.

---

### Transformations des variables

Pour chaque variable, plusieurs transformations usuelles ont été testées :

- Inverse : g(x) = 1/x  
- Logarithmique : g(x) = ln(x)  
- Carrée : g(x) = x²  
- Exponentielle : g(x) = eˣ  
- Racine carrée : g(x) = √x  

La transformation améliorant la **tendance** ou la **linéarité** a été retenue lorsque pertinente.  
À défaut, la variable a été conservée dans sa forme initiale.

---

### 1. Analyse exploratoire des données
- Analyse des distributions des variables
- Détection des valeurs aberrantes et problèmes de qualité des données
- Identification des premiers patterns liés aux sinistres

### 2. Modélisation de la fréquence
- Application de GLM pour modéliser le nombre de sinistres
- Distributions typiques
- Analyse des facteurs de risque liés aux assurés

### 3. Modélisation de la sévérité
- Application de GLM pour modéliser le coût des sinistres
- Distributions typiques
- Identification des déterminants des montants de sinistres

### 4. Sélection des variables
- Tests de significativité statistique
- Pertinence métier (actuarielle)
- Gestion de la multicolinéarité

### 5. Validation des modèles
- Mesures de qualité d’ajustement
- Analyse des résidus
- Tests de robustesse et de stabilité

### 6. Interprétabilité
- Analyse des coefficients
- Cohérence avec la logique actuarielle
- Traduction des résultats en insights métier

---

## Outils & Technologies

- **R**
- Modèles linéaires généralisés (GLM)
- Techniques statistiques
- Visualisation de données

---

## Structure du projet


- Code Tarification.r # Script principal de modélisation
- Sommaire de projet.pdf # Documentation et synthèse du projet


---

## Résultats clés

- Développement d’un modèle structuré de tarification actuarielle
- Identification des principaux facteurs influençant la fréquence et la sévérité des sinistres
- Mise en évidence du rôle de la modélisation dans les décisions de tarification
- Construction d’un workflow analytique reproductible et aligné avec les pratiques du secteur

---

## Pertinence métier

Ce projet illustre comment les assureurs :
- Évaluent les profils de risque
- Déterminent les primes en fonction des pertes attendues
- Équilibrent rentabilité et compétitivité

Il met en évidence la chaîne de valeur :
**Données → Modélisation → Décisions de tarification**

---

## Améliorations futures

- Intégration de modèles de machine learning pour comparaison
- Validation croisée et optimisation des modèles
- Développement d’un tableau de bord interactif
- Utilisation de données réelles à plus grande échelle

---

## Auteur

**Steve Soro**  
Analyste de Données Bilingue
Actuariat & Intelligence d'Affaires  
SQL • Python • Power BI • R  

---

## Contact

N’hésitez pas à me contacter pour échanger ou collaborer sur des projets similaires.
