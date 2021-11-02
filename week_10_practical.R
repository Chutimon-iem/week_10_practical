#generate data set
set.seed(246)
x = sort(rnorm(30, 0, 2))
noise = rnorm(30, 2, 2)
w = 0.4 * x + noise
cor(x,w)

y = sort(rnorm(15,0,1))
noise2 = rnorm(15,2,1)
z = 2.1 * y + noise2
cor(y,z)

ggplot(random_data, aes(x,w))+
  geom_point() +
  geom_smooth(method="lm" , se=FALSE) 


random_data = data.frame(w,x,y,z)
head(random_data)


#Q5 Model your outcome using W, X, Y, and Z.Do your results match your model params?
beta_0 = 1
beta_1 = 2
beta_2 = 3
beta_3 = 4
beta_4 = 5
error = (rnorm(30, 0, 0.6))
outcome = (beta_0 + beta_1 * w + beta_2 * x + beta_3 * y + beta_4 * z) + error
outcome

random_model = lm(outcome ~ ., data = random_data)
summary(random_model)


#Q6 Use PCA to reduce the dimensionality of your dataset.Can you explain why you don't need to include the outcome?

my_pcr = prcomp(random_data , scale=TRUE)
summary(my_pcr)
str(my_pcr)

#Q7 Use the bi-plot to visualize the contributions of your initial variables

fviz_pca_biplot(my_pcr)

#Q8 How efficient is the new lower-dimensional space representation at predicting the outcome? Do your results match your model params?
ev


cov_matrix  = cov(random_data)
cov_matrix

ev = eigen(cov_matrix)
ev$vectors

ev$values
ev$values/sum(ev$values)
#the first eiganvector capture 53.13% of total variance in the data

