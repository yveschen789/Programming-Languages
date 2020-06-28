import numpy as np
#Current state
I = np.matrix([[1, 0]])
#Transition Matrix
T = np.matrix([[.7, 0.3],
               [.1, 0.9]])
T1 = I * T
# After 1 hours
print (T1)
T2 = T1 * T
# After 2 hours
print (T2)
T3 = T2 * T
# After 3 hours
print (T3)

T4 = T3
for i in range(0,50):
    T4 = T4*T
    print(T4)