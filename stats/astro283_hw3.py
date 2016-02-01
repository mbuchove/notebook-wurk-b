
# coding: utf-8

# <h2>HW 3</h2>
# Matt Buchovecky
# Astro 283

# We wish to find and characterize the likelihood of certain parameters given the data, e.g. for flux:
# $$ P\left(F\mid\{D_{x,y}\},I\right) $$  
# Here are the givens:  
# $k=3.0\frac{e^-}{DN}$ is the gain  
# $N_e=kN_{DN}$ converts from digital counts to photoelectrons  
# Because your counting statistics are done on the actual 
# $\sigma_{DN}=\frac{1}{k}\sigma_N$  
# The parameters can be put into a vector $\vec z$
# $$ \vec z = \left(x_c, y_c, F, \sigma_{PSF}, B\right) $$

# <h2>Finding the probability of a set of parameters</h2>
# Since we are modelling the Point Spread Function to be a symmetric 2D gaussian with amplitude of the point flux $F$ at the true center, the expected signal at a location elsewhere will follow:  
# $$  f\left(x,y,\vec z\right) = Fexp{\left[-\frac{\left(x-x_c\right)^2+\left(y-y_c\right)^2}{2\sigma_{PSF}^2}\right]} + B $$  
# where B is a common background constant. The probability distribution for getting a signal given the flux parameter at a specific position follows a Poisson distribution:  \\
# \begin{eqnarray} 
# p\left(\vec{z}\mid D_{x,y}\right) &\propto& \prod_{x,y} \frac{f^De^{-f}}{D!} \\
# &\propto& \prod_{x,y} \exp{\left[-\frac{\left(D_{x,y}-f\left(x,y,\vec z\right)\right)^2}{f\left(x,y,\vec z\right)}\right]} \\
# &\approx\propto& \prod_{x,y} \exp{\left[-\frac{\left(D_{x,y}-f\left(x,y,\vec z\right)\right)^2}{D_{x,y}}\right]}
# \end{eqnarray}  
# 
# The first approximation above is because our numbers of counts are high, so we can approximate the Poisson distribution as a Gaussian distribution. In order to make this a chi squared problem, we have to make a further approximation that the variance is the measured "flux", not the actual flux. Note, when I use the quantity of flux, I am referring to the expected flux number, in data counts, for the same time as the time of observation  
# 
# Since we have a chi square problem now, we can maximize the likelihood of the parameters by doing a least squares fit to $p\left(\vec{z}\mid D_{x,y}\right)$. This can be done in python with scipy.optimize using the curve_fit function, which will output the best fit parameters, as well as the covariance matrix $\Sigma$
# 

# To marginalize the data, we integrate over every other parameter:
# $$ P\left(F\mid\{D_{x,y}\}\right) = \int p\left(\vec{z} \mid \{D_{x,y}\}, I\right)d\sigma dxdydB $$
# $$ P\left(x_c,y_c\mid\{D_{x,y}\}\right) = \int p\left(\vec{z} \mid \{D_{x,y}\}, I\right)dFd\sigma dB $$  
# 
# As with the discussion example, the marginal distribution of $F$ will result in a gaussian with a mean of the optimal flux, and width of the square root of the variance taken from the fit's covariance matrix. The joint marginal distribution of $x_c$ and $y_c$ will be a 2d gaussian centered at $\left(x_c, y_c\right) with a covariance matrix, which is the small subset of the larger covariance matrix
# 
# $$
# \Sigma^\prime = \left[
# \begin{array}{ccc}
# \sigma^{2}_{xx} & \sigma^{2}_{xy}\\
# \sigma^{2}_{yx} & \sigma^{2}_{yy}
# \end{array}
# \right]
# $$
# 

# <h1> Perform the fit and plot the data! </h1>

# In[111]:

import numpy
from astropy.io import fits 
from scipy import optimize
from matplotlib import pyplot 
get_ipython().magic('matplotlib inline')


# In[112]:

star_fits = fits.open("./hw3prob1-data.fits")
star_data = star_fits[0].data
star_rav = star_data.ravel()
print(star_fits[0].header)
print(star_data)


# In[113]:

fig = pyplot.figure()
pyplot.imshow(star_data)
pyplot.xlabel('x (pixels)')
pyplot.ylabel('y (pixels)')
pyplot.title("FITS data")
cbar = pyplot.colorbar(label='DN')
cbar.solids.set_edgecolor('face')


# In[114]:

def symm_2d_gaussian(pos, F, xc, yc, sigma, B):
    return F*numpy.exp(-((pos[0]-xc)**2+(pos[1]-yc)**2)/(2*sigma**2)) + B
    


# In[115]:

x_bins = star_data.shape[0]
y_bins = star_data.shape[1]

# Create x and y indices
x_arr = numpy.linspace(0, x_bins-1, x_bins)
y_arr = numpy.linspace(0, y_bins-1, y_bins)
x_arr, y_arr = numpy.meshgrid(x_arr, y_arr)
coords = x_arr, y_arr

x_rav = x_arr.ravel()
y_rav = y_arr.ravel()


# In[119]:

init_guess = (50, 130, 130, 20, 100)
sigma_array = numpy.sqrt(star_data)
p_opt, p_cov = optimize.curve_fit(symm_2d_gaussian, (x_arr.ravel(), y_arr.ravel(), star_data.ravel(), init_guess, sigma_array.ravel()))


# In[ ]:

print("param name\tF \t\tx_center\ty_center\tPSF\t\tbackground")
param_string = "optimal value\t"
for i in range(0, len(p_opt)):
    param_string = param_string + "{0:0.5f} \t".format(p_opt[i])
print(param_string)
sigma_string = "variance\t"
for i in range(0, len(p_opt)):
    sigma_string = sigma_string + "{0:0.5f} \t".format(p_cov[i][i])
print(sigma_string)
print("\nCovariance matrix:")
print(p_cov)


# In[ ]:

sigma_prime = numpy.matrix(p_cov[1:3,1:3])
sigma_inv = sigma_prime.I
det_sigma = numpy.linalg.det(sigma_prime)
print("determinant of sigma prime")
print(det_sigma)
print("inverse of sigma prime")
print(sigma_inv)
x_0_matrix = numpy.matrix([[125.973], [132.452]])


# As stated above, the marginal probability of the flux will be: 
# $$ p\left(F\mid\{D_{x,y}\}\right) = \frac{1}{\sqrt{2\pi}\sigma_{F}}e^{-\frac{\left(F-F_0\right)^2}{2\sigma_{F}^2}} $$  
# which we can write as $F_0 = 26.193 \pm 0.640$  
# <br>
# Now we can find the joint marginal distribution for the center position $\vec{x} = \left(x_c, y_c\right)$  
# The fit gives optimal parameters and covariance:  
# $\vec{x}_0 = \left(125.973, 132.452\right)$ 
# $\;$  
# $\Sigma^\prime = \left[
# \begin{array}{ccc}
# 4.477\times 10^{-2} & 4.123\times 10^{-4}\\
# 4.122\times 10^{-4} & 4.484\times 10^{-2}
# \end{array}
# \right]
# $  
# The form of the joint marginal probability distribution is here, the $\Sigma^{\prime}$ matrix, its determinant, and inverse are found and shown above:   
# $$ p\left(\vec{x}\mid\{D_{x,y}\}\right) = \frac{1}{\sqrt{\left(2\pi\right)^2\left|{\Sigma^\prime}\right|}}\exp{\left[-\frac{1}{2}\left(\vec{x}-\vec{x}_0\right)^T\Sigma^{\prime -1}\left(\vec{x}-\vec{x_0}\right)\right]} $$  
# 
# 

# In[ ]:

x_0_matrix = numpy.matrix([[125.973], [132.452]])
mat_prod = numpy.dot(x_0_matrix.T, numpy.dot(sigma_inv, x_0_matrix))
print(mat_prod)
print(0.5*x_0_matrix.T*sigma_inv*x_0_matrix)


# In[ ]:

def joint_marginal_dist_2(pos, center, cov_mat):
    if type(pos) is not numpy.matrix:
        pos = numpy.matrix(pos)
    return 1/numpy.sqrt((2*numpy.pi)**2*cov_mat.I)*numpy.exp(-0.5*(pos-center).T*cov_mat*(pos-center))
# mat1 * mat2 equivalent to numpy.dot(mat1, mat2)


# <h3>Check the fit</h3>

# In[ ]:

#center_prob = 
#center_pos = 
#numpy.matrix([[x_arr], [y_arr]])
for i in range(0, len(x_arr)):
    for j in range(0, len(y_arr)):
        
        
pyplot.contour(x_arr, y_arr, joint_marginal_dist_2([[x_arr], [y_arr]], x_0_matrix, sigma_prime))


# In[ ]:

import warnings
 


# In[ ]:

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    pyplot.contourf(star_data, edgecolors='face')
    pyplot.contour(x_arr, y_arr, symm_2d_gaussian((x_arr,y_arr), *p_opt), colors='black')
    cbar.solids.set_edgecolor('face')
    pyplot.axis([100,150,100,160])
    pyplot.xlabel('x [pix]')
    pyplot.ylabel('y [pix]')


# <h3>Subtract the source fit</h3>

# In[ ]:

blank_sky = star_data - symm_2d_gaussian((x_arr,y_arr), *p_opt)
contour = pyplot.contourf(blank_sky, edgecolors='face')
pyplot.xlabel('x')
pyplot.ylabel('y')
pyplot.title("subtracted map")
cbar = pyplot.colorbar(label='DN')


# In[ ]:

Now 

