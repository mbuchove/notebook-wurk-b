import numpy as np
from csv import reader
from scipy.optimize import curve_fit

data = reader(open('hw5prob2-data.txt','rt'), delimiter=' ')
x_dat = []
y_dat = []
sig = []
for row in data:
	if row[0] == 'x':
		continue
	else:
		x_dat.append(float(row[0]))
		y_dat.append(float(row[1]))
		sig.append(float(row[2]))
		
#print x_dat

def poly(x,n,L):
    if n+1 != len(L):
        return "Error: need ", n+1, " parameters!"        
    out = 0
    for k in range(0,n+1):
        out += L[n-k]*np.power(x,n-k)
    return out

f_3 = lambda x,L3,L2,L1,L0: poly(x,3,[L3,L2,L1,L0])
f_2 = lambda x,L2,L1,L0: poly(x,2,[L2,L1,L0])

popt_3, pcov_3 = curve_fit(f_3, x_dat, y_dat, p0=[1,1,1,1], sigma=sig)
popt_2, pcov_2 = curve_fit(f_2, x_dat, y_dat, p0=[1,1,1], sigma=sig)

print(popt_3)
print(popt_2)

def chi_sq(x, y, sigma, fun, *params):
    out = 0
    for i in range(0,len(x)):
        out += (y[i] - fun(x[i], *params))**2/(sigma[i]**2)
    return out

def gaussian_f(x, x0, cov):
    if type(cov) is not np.matrix:
        cov = np.matrix(cov)
    vec = np.matrix(x - x0)
    n_p = len(vec)
    return float(1/(np.sqrt((2*np.pi)**n_p*np.linalg.det(cov)))*np.exp(-0.5*vec*cov.I*vec.T))
	
#iters = 1000000
f = open('gaussian_samples4.txt','w')
ctr = 0
while ctr < 100000:
	u_rand = np.random.rand()
	x_rand = [np.random.uniform(popt_3[0]-2,popt_3[0]+2), 
			  np.random.uniform(popt_3[1]-.5,popt_3[1]+.5), 
			  np.random.uniform(popt_3[2]-.05,popt_3[2]+.05), 
			  np.random.uniform(popt_3[3]-.005,popt_3[3]+.005)]
	if u_rand < gaussian_f(x_rand, popt_3, pcov_3)/gaussian_f(popt_3, popt_3, pcov_3):
		f.write(str(x_rand)+'\n')
		ctr += 1
	#if ctr in [200,400,600,800,1000]:
		#print(ctr, "samples obtained!")
f.close()