Scala's Pattern Recognition and Machine Learning
====

![image](https://img.shields.io/badge/Gradle-7-red.svg)
![image](https://img.shields.io/badge/Java-SE13-red.svg)
![image](https://img.shields.io/badge/Python-3.8-red.svg)
![image](https://img.shields.io/badge/Scala-3.0-orange.svg)
![image](https://img.shields.io/badge/license-BSD%203--Clause-darkblue.svg)

sample codes of machine learning models and algorithms written in Scala, for the educational purpose in data science.

## Documents

- [Scalaで実装するパターン認識と機械学習 (PDF)](https://nextzlog.dev/mine.pdf) [(HTML)](https://nextzlog.dev/mine.html)

## Chapter 1

### Nearest Neighbors

```sh
$ java -jar build/libs/mine.jar KNN
$ java -jar build/libs/mine.jar LR
$ java -jar build/libs/mine.jar NBC
```

![KNN](images/knn.class.svg)

### Linear Regression

![Regression 1](images/lbf.power.svg)
![Regression 2](images/lbf.gauss.svg)

### Naive Bayes Classifier

![NBC 1](images/nbc.jmap2.svg)
![NBC 2](images/nbc.jmap8.svg)

## Chapter 2

```sh
$ java -jar build/libs/mine.jar MLP
$ java -jar build/libs/mine.jar SGD
$ java -jar build/libs/mine.jar RNN
```

### Multi-Layer Perceptron

![MLP 1](images/mlp.class.svg)
![MLP 2](images/mlp.const.svg)

### Plain SGD vs AdaDelta

![SGD 1](images/sgd.avoid.svg)
![SGD 2](images/sgd.speed.svg)

### Recurrent Neural Network

![RNN](images/rnn.phase.svg)

## Chapter 3

```sh
$ java -jar build/libs/mine.jar SVM
```

### Linear SVM

![Linear SVM 1](images/svm.line1.svg)
![Linear SVM 2](images/svm.line2.svg)

### Kernel SVM

![Kernel SVM 1](images/svm.kern1.svg)
![Kernel SVM 2](images/svm.kern2.svg)

## Chapter 4

```sh
$ java -jar build/libs/mine.jar DT
```

### Decision Tree

![Decision Tree 1](images/id3.plain.svg)
![Decision Tree 2](images/id3.prune.svg)

### AdaBoost

![AdaBoost](images/id3.ada50.svg)

### Ensemble

![Ensemble](images/id3.bag50.svg)

## Chapter 5

```sh
$ java -jar build/libs/mine.jar LDA
```

### Latent Dirichlet Allocation

```
0,7,14,21,28,35,42,49,56,63,70,77
5,10,15,20,25,30,40,45,50,55,60,65,75,80
3,6,9,12,18,24,27,33,36,39,48,51,54,57,66,69,72,78
2,4,8,16,22,26,32,34,38,44,46,52,58,62,64,68,74,76
```

## Chapter 6

```sh
$ java -jar build/libs/mine.jar GMM
```

### K-Means

![K-Means 1](images/gmm.km.k2.svg)
![K-Means 2](images/gmm.km.k3.svg)

### EM Algorithm

![EM Algorithm 1](images/gmm.em.k2.svg)
![EM Algorithm 2](images/gmm.em.k3.svg)

### VB-EM Algorithm

![VB-EM Algorithm 1](images/gmm.vb.k2.svg)
![VB-EM Algorithm 2](images/gmm.vb.k3.svg)

## Dependencies

- Python3.8
- numpy
- scipy
- matplotlib
- cartopy
- geos

## Build

```sh
$ gradle build
```

## Contribution

Feel free to make issues at [nextzlog/todo](https://github.com/nextzlog/todo).
Follow [@nextzlog](https://twitter.com/nextzlog) on Twitter.

## License

### Author

[無線部開発班](https://nextzlog.dev)

### Clauses

[BSD 3-Clause License](LICENSE.md)
