Projet avec R
=============

Q.1
---
.. code-block::R

    load('REPERTOIRE_DE_TRAVAIL/titanic_train.Rdata')


Q.2
---
donner le nombre d’observations 594 Observation et 12 variables
.. code-block::R

    dim(train)

donner le nom des variables et dire si elles sont quantitatives ou qualitatives
.. code-block::R

    str(train) #Factor pour les variables qualitatives / int et num pour les variables quantitatives
 
donner le nombre de valeurs manquantes.
.. code-block::R

    colSums(is.na(train))

Quelles sont les variables avec le plus de données manquantes ?
.. code-block::R

    sort(colSums(is.na(train)), decreasing = TRUE)[1:3]


Q.3 
---
décrire S, Sx, P et A de manière appropriée.

* S (Survived variable qualitative binaire)
  * 1 veut dire que individu est survécu **(221|37%)**
  * 2 veut dire que individu est mort **(373|63%)**

.. code-block::R

    table(train$Survived)
    prop.table(table(train$Survived))

* Sx (Sex variable qualitative binaire)
  * 1 female pour les femmes **(301|34%)**
  * 2 male pour les hommes **(393|66%)**

.. code-block::R

    table(train$Sex)
    prop.table(table(train$Sex))


* P (Pclass variable qualitative ordinale)
  * 1 pour la classe haute **(139|23%)**
  * 2 pour la classe moyenne **(124|21%)**
  * 3 pour la classe economique **(331|56%)**

.. code-block::R

    table(train$Pclass)
    prop.table(table(train$Pclass))
    
    
* A (Age variable quantitative continue)
  * avec **121** valeurs manquantes
  * moyenne: **29.58%**, médiane: **28**, variance: **206.29**, ecart-type: **14.36**

.. code-block::R

    mean(train$Age, na.rm = T) #moyenne
    median(train$Age, na.rm = T) #médiane
    var(train$Age, na.rm = T) #variance
    sd(train$Age, na.rm = T) #écart-type
    

Q.4 
---
creation variable cAge quantitative à qualitative

.. code-block::R

    cAge = cut(train$Age, breaks=c(0,20,40,60,80))

