# Forecast Algorithm
# Dr.V.
# VERSION : 1.6
#

import warnings

import numpy as np
import pandas as pd
#from sklearn import linear_model
import statsmodels.formula.api as sm
from plotly import tools
import plotly.offline as py
from plotly.offline import download_plotlyjs, init_notebook_mode, iplot
import plotly.graph_objs as go
init_notebook_mode(connected=True)

from scipy.optimize import curve_fit
import math

# test case 6
# viewership_6.csv
video_length = float(827.753)
impressions = int(707204)
three_sec_view = int(223074)
ten_sec_view = int(75711)
thirty_sec_view = int(35619)
ninty_five_view = int(8604)

data1 = pd.read_csv('test.csv')

temp = pd.DataFrame({'Retention by Moment': [1], 'x': [0], 'length_point':[1e-10],
                'length_point_square': [1e-20], 'reversed_length_point': 1, 'Retention_by_Moment': [1],
                    'log_length_point': [math.log(1e-10)]})

data1['x']=range(41)[1:]
denominator = ninty_five_view/data1['Retention by Moment'][37]
# pick the 95% viewership as total viewership
#print(denominator)
#data1['views'] = data1['Retention by Moment']*denominator
data1['length_point'] = 0.025*data1['x']*video_length
data1['length_point_square'] = data1['length_point']**2
data1['Retention_by_Moment'] = data1['Retention by Moment']
data1['reversed_length_point'] = 1/data1['length_point']
data1['log_length_point'] = np.log(data1['length_point'])

df = pd.concat([temp, data1]).sort_values(['length_point']).reset_index(drop=True)
# now df is dataframe with actual viewership sorted by time point


model_list = []
color_list = ['blue', 'yellow', 'red', 'green', 'brown', 'purple', 'black', 'orange']
function_list = []
name_list = ['lm y~x', 'lm y~1/x', 'lm y~logx', 'nls y~a/x+b*x', 'nls y~a+b*logx', 'nls y~exp(a+b*x)', 'nls y~a/x+b', 'nls g,a']

# multiple models
model_1 = sm.ols(formula="Retention_by_Moment ~ length_point", data=df).fit()
'''
model_1 = linear_model.LinearRegression()
model_1.fit(df['length_point'].reshape(-1, 1), df['Retention by Moment'])
'''
model_list.append(model_1)
function_list.append(0)

model_2 = sm.ols(formula="Retention_by_Moment ~ reversed_length_point", data=df).fit()
'''
model_2 = linear_model.LinearRegression()
model_2.fit(1/df['length_point'].reshape(-1, 1), df['Retention by Moment'])
'''
model_list.append(model_2)
function_list.append(0)

model_3 = sm.ols(formula="Retention_by_Moment ~ log_length_point", data=df).fit()
'''
model_3 = linear_model.LinearRegression()
model_3.fit(np.log(df['length_point']).reshape(-1, 1), df['Retention by Moment'])
'''
model_list.append(model_3)
function_list.append(0)


def model4_func(x, a, b):
    return (1/x)*a+b*x

model_4, pcov_4 = curve_fit(model4_func, df['x'] + 1 , df['Retention by Moment'], p0=[1,1])
model_list.append(model_4)
function_list.append(model4_func)

print(model_4)
import sys
sys.exit()



def model5_func(x, a, b):
    return a+(b*np.log(x))

model_5, pcov_5 = curve_fit(model5_func, df['x']+1, df['Retention by Moment'], p0=[model_3.params['Intercept'], model_3.params['log_length_point']])
model_list.append(model_5)
function_list.append(model5_func)


def model6_func(x, a, b):
    return np.exp(a+b*x)

model_6, pcov_6 = curve_fit(model6_func, df['x']+1, df['Retention by Moment'], p0=[0, 0])
model_list.append(model_6)
function_list.append(model6_func)

def model7_func(x, a, b):
    return (a/x)+b

model_7, pcov_7 = curve_fit(model7_func, df['x']+1, df['Retention by Moment'], p0=[1, 1])
model_list.append(model_7)
function_list.append(model7_fusnc)

def model8_func(x, a, g):
    return (g/a)*(x/a)**(g-1)*np.exp(-(x/a)**g)

model_8, pcov_8 = curve_fit(model8_func, df['x']+1, df['Retention by Moment'], p0=[1, 1])
model_list.append(model_8)
function_list.append(model8_func)

total_ss = np.sum((df['Retention by Moment']-np.mean(df['Retention by Moment']))**2)


fig = tools.make_subplots(rows=1, cols=1, specs=[[{}]], print_grid=False)


for k in range(len(model_list)):

    if k<=2: # for lm model
        residual_ss = 0
        for row in model_list[k].resid:
            residual_ss += row**2
        r_squared = 1 - (residual_ss / total_ss)

        if k ==1: # for 1/x
            xdata = df['reversed_length_point']
        elif k==2: # for log(x)
            xdata = df['log_length_point']
        else:
            xdata = df['length_point']

        fig.append_trace(go.Scatter(
                        x=list(df['length_point']),
                        y=model_list[k].predict(xdata),
                        mode='lines',
                        line=dict(color=color_list[k], width=3),
                        opacity = 0.5,
                        hoverinfo="x+y",
                        name=name_list[k]+": "+str(round(r_squared, 4)*100)+"% good",
                        showlegend=True
                        ), 1, 1)


    else: # for nls model
        residual_ss = 0
        for row in range(len(df['length_point'])):
            residual_ss += (df['Retention by Moment'][row]- function_list[k](df['length_point'][row], *model_list[k]))**2
        r_squared = 1 - (residual_ss / total_ss)

        fig.append_trace(go.Scatter(
                        x=list(df['length_point']),
                        y=function_list[k](df['length_point'], *model_list[k]),
                        mode='lines',
                        line=dict(color=color_list[k], width=3),
                        opacity = 0.5,
                        hoverinfo="x+y",
                        name=name_list[k]+": "+str(round(r_squared, 4)*100)+"% good",
                        showlegend=True
                        ), 1, 1)


fig.append_trace(go.Scatter(x=list(df['length_point']),
                            y=list(df['Retention by Moment']),
                            mode='markers',
                            marker=dict(color='grey', size = 8),
                            opacity = 0.5,
                            hoverinfo="x+y",
                            name='Ground Truth',
                           showlegend=True), 1,1)

fig['layout'].update(title = 'Retention at each Time Stamp', xaxis=dict(title='Time stamp(s)'), yaxis=dict(title='Retention'))
page_name = 'retention_curves'
py.plot(fig, auto_open=True, filename=page_name)
