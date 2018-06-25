# -*- coding: utf-8 -*-
"""
Created on Wed Jun 13 12:56:23 2018

@author: Administrator
"""

import numpy as np
annual_salary=float(input("enter your annual salary"))
portion_saved=float(input("How much amount to be saved in decimal"))
total_cost=float(input("Total cost for your Dream Home"))
portion_down_payment=0.25*total_cost
current_savings=0
r=0.04

def cost(a,b,c):
    monthly_salary=a/12
    current_savings=monthly_salary*(b/100)
    if(current_savings==portion_down_payment):
        months=c/current_savings
        return months
d=cost(annual_salary,portion_saved,total_cost)
print("Number of months required=",d)