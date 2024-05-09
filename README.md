E-commerce customer segmentation.
==============================

The aim of this project is to strategically segment the customers of an e-commerce platform specializing in the sale of all-occasion. Using exploratory data analysis techniques in Tableau, it will be uncovered some underlying customer patterns and profiles. Cluster analysis through R will define distinct customer segments based on transactional behaviour, complemented by LDA. The project will ultimately employ RFM analysis to further segment the customers. 

Project Organization
------------

    ├── LICENSE
    ├── README.md          <- Project information.
    ├── data
    │   └── sales_data     <- Data of all the insurance company customers. 
    │
    ├── docs               <- A default Sphinx project.
    │
    ├── models             
    │   ├── LazyPredict    <- Library uses to compare initial performances of all clasiffiers. 
    │   ├── RandomForest   <- Hyperparameter tuning and best model selection for the 1st classfier. 
    │   ├── SVC            <- Hyperparameter tuning and best model selection for the 2st classfier. 
    │   └── Neural Network <- Hyperparameter tuning and best model selection for the 3st classfier. 
    ├── Notebooks
    │   ├── 1              <- Data cleaning and analysis code. 
    │   ├── 2              <- Machine Learning Models code. 
    │   └── 3              <- Complete code.
    │
    ├── reports            <- Detailed report with recomendations to the company. Following CRISP-DM methodolody.
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable so src can be imported
    └──  src                <- Source code for use in this project.
        ├── __init__.py    <- Makes src a Python module.
        │
        ├── data           <- Download the data.
        │   └── sales_data
        │
        │
        └── models         <- Scripts to train models and then use trained models to make
            │                 predictions 
            ├── train_test_split.py
            ├── lazy_predict.py.
            ├── random_forest.py.
            ├── SVC.py
            └── neural_network.py



--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
