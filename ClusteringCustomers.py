import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
sns.set()
sns.set_style("darkgrid")


""" The main objective of this script is to
    cluster the customer into different classes
    depending on their partworth utilities."""



# First we import the data estimated using the 
# ChoiceModelR package in R
estbetas = pd.read_csv('C:/Users/amade/OneDrive/Documentos/RBetas.csv')
estbetas.columns = ['Id','4GB','8GB','16GB','5inch','6inch','7inch','black','white','silver','79','99','119','139','none']

# We then start with the clustering techniques:
# Prior is important to drop the Id column, since
# it disturb the results greatly:
estbetas = estbetas.drop('Id',axis=1)
    
########
# K-Means algorithm:
from kneed import KneeLocator
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.decomposition import PCA

# The elbow method:
wcss=[]
for i in range(1, 11):
    kmeans = KMeans(n_clusters=i, n_init=10)
    kmeans.fit(estbetas)
    wcss.append(kmeans.inertia_)

plt.plot(range(1, 11), wcss)
plt.title('Selecting the Number of Clusters using the Elbow Method')
plt.xlabel('Clusters')
plt.ylabel('WCSS')
plt.show()

kl = KneeLocator(range(1, 11), wcss, curve="convex", direction="decreasing")
kl.elbow
# The rule of thumb is to choose the number of clusters
# where the elbow of the graph is recorded. In this case
# we can observe that n=4 is a good number.

# The silhoutte coefficient:
silhouette_coefficients = []
# Notice you start at 2 clusters for silhouette coefficient
for k in range(2, 11):
    kmeans = KMeans(n_clusters=k)
    kmeans.fit(estbetas)
    score = silhouette_score(estbetas, kmeans.labels_)
    silhouette_coefficients.append(score)


plt.plot(range(2, 11), silhouette_coefficients)
plt.xticks(range(2, 11))
plt.xlabel("Number of Clusters")
plt.ylabel("Silhouette Coefficient")
plt.title("Selecting number of clusters using the Silhouette Coefficient")
plt.show()
# According to the silhoutte method n=6 could be
# a good number of clusters.


# Lets use n clusters and see the results that we obtain:
kmeans = KMeans(n_clusters=3, n_init=15,random_state=3)
kmeans.fit(estbetas)
kmeans.inertia_
kmeans.cluster_centers_
kmeans.n_iter_
yhat = kmeans.predict(estbetas)

estbetasclustered = pd.concat([estbetas,pd.DataFrame(yhat)],axis=1)
estbetasclustered.columns = ['4GB','8GB','16GB','5inch',
                             '6inch','7inch','black','white',
                             'silver','79','99','119','139',
                             'none','cluster']


# Lets see what happens if we use Principal Component Analysis
# to reduce the space dimension to 2, and check how well the
# clusters are splitted.
reduced_data = PCA(n_components=2).fit_transform(estbetas)
kmeans = KMeans(init="k-means++", n_clusters=3, n_init=10)
kmeans.fit(reduced_data)
label = kmeans.predict(reduced_data)
label = [str(i) for i in label]
points = pd.DataFrame([reduced_data[:,0],reduced_data[:,1],label])
points = points.T
sns.scatterplot(x=points.iloc[:,0],y=points.iloc[:,1],
                hue=points.iloc[:,2]).set_title('Clustering Customers using K-Means')
plt.show()


# Step size of the mesh. Decrease to increase the quality of the VQ.
h = 0.02  # point in the mesh [x_min, x_max]x[y_min, y_max].

# Plot the decision boundary. For that, we will assign a color to each
x_min, x_max = reduced_data[:, 0].min() - 1, reduced_data[:, 0].max() + 1
y_min, y_max = reduced_data[:, 1].min() - 1, reduced_data[:, 1].max() + 1
xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))

# Obtain labels for each point in mesh. Use last trained model.
Z = kmeans.predict(np.c_[xx.ravel(), yy.ravel()])

# Put the result into a color plot
Z = Z.reshape(xx.shape)
plt.figure(1)
plt.clf()
plt.imshow(
    Z,
    interpolation="nearest",
    extent=(xx.min(), xx.max(), yy.min(), yy.max()),
    cmap=plt.cm.Paired,
    aspect="auto",
    origin="lower",
)

plt.plot(reduced_data[:, 0], reduced_data[:, 1], "k.", markersize=2)
# Plot the centroids as a white X
centroids = kmeans.cluster_centers_
plt.scatter(
    centroids[:, 0],
    centroids[:, 1],
    marker="x",
    s=169,
    linewidths=3,
    color="w",
    zorder=10,
)
plt.title(
    "K-means clustering on the eook dataset (PCA-reduced data)\n"
    "Centroids are marked with white cross"
)
plt.xlim(x_min, x_max)
plt.ylim(y_min, y_max)
plt.xticks(())
plt.yticks(())
plt.show()


# Lets try to make boxplots of the different clusters that we have found.
import seaborn as sns
estbetas_long = pd.melt(estbetas)
#colors=[np.repeat('red',600),np.repeat('lightblue',600),
#        np.repeat('lightgreen',600),np.repeat('pink',800),
#                 np.repeat('yellow',200)]
#estbetas_long['colors']=np.hstack(colors)

zhat = yhat
for i in range(0,13):
    zhat = np.append(zhat,yhat)

estbetas_long['cluster'] = zhat  
my_pal = {"4GB": "lightgreen","8GB": "lightgreen", "16GB": "lightgreen",
          '5inch':'lightblue','6inch':'lightblue','7inch':'lightblue',
          'black':'r','white':'r','silver':'r',
          '79':'lightyellow','99':'lightyellow','119':'lightyellow','139':'lightyellow',
          'none':'w'}
         
sns.boxplot(x=estbetas_long["variable"], y=estbetas_long["value"],
            linewidth=1,palette=my_pal).set_title('Box plots of Individual utilities \n using Hierarchichal Bayes MNL model')
plt.show()

sns.boxplot(x=estbetas_long["variable"], y=estbetas_long["value"],
            hue=estbetas_long['cluster'],linewidth=1).set_title('Clustering using K-Means')
plt.show()





###############
# Agglomerative Hierarchical Clustering:
import scipy.cluster.hierarchy as sch
from sklearn.cluster import AgglomerativeClustering
    

# create dendrogram
dendrogram = sch.dendrogram(sch.linkage(estbetas, method='ward'))
# create clusters
hc = AgglomerativeClustering(n_clusters=4, affinity = 'euclidean', linkage = 'ward')
# save clusters for chart
y_hc = hc.fit_predict(estbetas)



reduced_data = PCA(n_components=2).fit_transform(estbetas)
hc = AgglomerativeClustering(n_clusters=3, affinity = 'euclidean', linkage = 'ward')
y_hc = hc.fit_predict(estbetas)
y_hc = [str(i) for i in y_hc]
points = pd.DataFrame([reduced_data[:,0],reduced_data[:,1],y_hc])
points = points.T
sns.scatterplot(x=points.iloc[:,0],y=points.iloc[:,1],
                hue=points.iloc[:,2]).set_title('Clustering Customers using \n Agglomerative Hierarchical Clustering')
plt.show()


z_hc=y_hc
for i in range(0,13):
    z_hc=np.append(z_hc,y_hc)

estbetas_long['cluster']=z_hc
sns.boxplot(x=estbetas_long["variable"], y=estbetas_long["value"],
            hue=estbetas_long['cluster'],linewidth=1).set_title('Clustering Customers using \n Agglomerative Hierarchical Clustering')
plt.show()






















