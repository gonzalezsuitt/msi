N=1000  # Définition de N, la taille de la population
X_min=0   # Définition de X_min
X_max=100   # Définition de X_max
sample(X_min:X_max, 10)   # commande de tirage aléatoire de 10 entiers entre X_min et X_max
X=rep(sample(X_min:X_max, N, replace = TRUE))    # création d'une population de taille N. L’option Replace=true permet à deux individus de se voir attribuer la même valeur
hist(X)   # Visualisation de la distribution de X dans la population
hist(X, breaks = 20)
mean(X) 
 
n=10   # Définition de n, la taille de l'échantillon
A=sample(1:N, n)    #Tirage aléatoire d'un échantillon de n individus
X[A]   # valeur prise par la variable X pour les individus de l'échantillon A
mean(X[A]) # Moyenne de la variable X pour les individus de l'échantillon A 
 
k=1000   # Définition de k, le nombre d'échantillons à tirer
X_bar=rep(0,k) 
 
# tirage de k échantillons de taille n (loop).  
 
for (i in 1:k){
  A=sample(1:N,n)
  A
  X_bar[i]=mean(X[A])
} 
 
X_bar   #le vecteur X_bar contient les moyennes de chaque échantillon
hist(X_bar, breaks = 20)    # Visualisation de la ditribution des moyennes des échantillons
mean(X_bar)