import pandas as pd
from pandas import Series, DataFrame
from sklearn import linear_model
from sklearn.metrics import confusion_matrix
import numpy as np
from sklearn.linear_model import LogisticRegression
from numpy import *
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import PolynomialFeatures, OneHotEncoder
from sklearn.preprocessing import StandardScaler

from sklearn.preprocessing import LabelEncoder


def table_x(file_users, file_logs):
    file_users['badge']=LabelEncoder().fit_transform(file_users["badge"])
    file_users["seconds"]=file_logs.groupby(["user_id"]).sum()
    file_users=file_users.fillna(0)
    return file_users
        
def table_y(file_y): 
    file_y["y"]=LabelEncoder().fit_transform(file_y["y"])
    return file_y
    
def table_test_x(test_users, test_logs):
   # test_users.index=Series(range(30000,60000))
    logs_seconds=test_logs.groupby(["user_id"]).sum()
    test_users=test_users.merge(logs_seconds,on="user_id", how="left")
    test_users['badge']=LabelEncoder().fit_transform(test_users["badge"])
    test_users=test_users.fillna(0)
    return test_users


class UserPredictor():
    def __init__(self):
        self.feature= ['age','past_purchase_amt',"badge","seconds"] 
        self.pipline= Pipeline([('scale', StandardScaler()),
                                
                                ("pf", PolynomialFeatures(degree=4, include_bias=False)),
                                ("lr", LogisticRegression())])
    
    def fit(self, train_users, train_logs, train_y):
        global result
        x=table_x(train_users,train_logs)[self.feature].values
        y=table_y(train_y)['y'].values
        result=self.pipline.fit(x,y)
        
    def predict(self, test_users, test_logs):
        x_test=table_test_x(test_users, test_logs)[self.feature].values
        y_predict=result.predict(x_test)
        bool_predict = np.array(y_predict, dtype= bool)
        return bool_predict




