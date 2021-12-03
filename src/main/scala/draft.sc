// breeze linear algebra operations


// creation

import breeze.linalg.{DenseMatrix, DenseVector, Transpose, diag, linspace}
import breeze.stats.distributions.Rand

// matrices
val m0 = DenseMatrix.ones[Double](5, 5)
val m1 = DenseMatrix.tabulate[Double](5, 5)((i, j) => i + j)
val m2 = DenseMatrix((1.0, 2.0), (3.0, 4.0))
val m3 = new DenseMatrix(2, 2, Array(1.0, 2.0, 3.0, 4.0))
val eyeM = DenseMatrix.eye[Double](3)
val diagM = diag(DenseVector(1.0, 2.0, 3.0))
// gaussian distribution
val randM = DenseMatrix.rand[Double](2, 3, rand = Rand.gaussian)
randM.cols
randM.rows

// vectors
val v0 = DenseVector.ones[Double](5)
val v1 = DenseVector.fill(5)(2.0)
val v2 = linspace(0, 100, 101) // start, stop, num
// vectors are by default column vectors
// row vectors are Transpose[DenseVector]
val v3: Transpose[DenseVector[Double]] = DenseVector(1.0, 2.0, 3.0).t
val v4 = new DenseVector(Array(1.0, 2.0, 3.0, 4.0))
// uniform distribution [0,1)
val randV = DenseVector.rand(4, rand = Rand.uniform)
randV.length

// I/O and serialization
// serialization is provided by Java

import breeze.linalg.{lowerTriangular, upperTriangular, tile}
import breeze.linalg.{csvread, csvwrite}
import java.io.File

val file: File = new File("./m0.csv")
csvwrite(file, diagM)
val m0Recovered = csvread(file)

// indexing and slicing(and assignment)
// all these methods are invoked by apply(), so you can press ctrl+P on the parenthesis to view them
diagM(0, 0)
diagM(0 to 1, 1 to 2)
diagM(::, 1 to 0 by -1) // step can be negative(on column index)
diagM(0 to -1, ::) // -1 means the last element

diagM(0 to 2, 0) := 5.0
diagM
diagM(0 to 2, 0) := DenseVector(1.0, 2.0, 3.0) // only available for column assignment
diagM

// other common manipulation on matrices and vectors
diagM.reshape(1, 9)
diagM.toDenseVector // flatten
upperTriangular(randM)
lowerTriangular(randM)
val randMCopy = randM.copy
val diagVector: DenseVector[Double] = diag(m2)
DenseMatrix.vertcat(m0, m1)
DenseMatrix.horzcat(m0, m1)
DenseVector.vertcat(v0, v1)
tile(v0, 1, 2)

// numeric operations

import breeze.linalg.{max, min, argmax, argmin}
// +, *, /, and inplace version
m0 + m1
m0 * m1 // matrix multiplication
m0 *:* m1 // elementwise multiplication
m0 /:/ m1 // elementwise multiplication
v0 dot v1
v0.t * v1
m0 += m1
m0
m0 *= m1
m0
// comparison
val lts = m0 <:< m1 // ...
val equals = m0 :== m1
//
max(m1)
argmax(m1) // index of max
min(m1) // index of max
argmin(m1) // index of max

// boolean/logical operations

import breeze.linalg.{any, all}

val b0: DenseMatrix[Boolean] = m0 <:< 0.0
val b1: DenseMatrix[Boolean] = m1 >:> 2.0
b0 &:& b1
b0 |:| b1
b0 ^^ b1
!b0
any(b0) // exist
all(b1) // forall

// rounding and signs
import breeze.numerics.{round, ceil, floor, signum, abs}

round(randM)

// numeric functions


// linear algebra operations

import breeze.linalg.{det, inv, pinv, norm, eig, eigSym, svd, rank}


val A = DenseMatrix.eye[Double](3)
val b = DenseVector.zeros[Double](3)
val x = A \ b // solve the equation Ax = b

b.t
det(A)
inv(A)
pinv(A) // Moore-Penrose Pseudoinverse
norm(b) // Vector Frobenius Norm

// on eigen values

import breeze.linalg.eig.{Eig}

eigSym(A) // Eigenvalues (Symmetric)
// the following lines used unapply method
val Eig(er, ei, vr) = eig(A)
er
ei
vr // eigen vector

// on SVD

import breeze.linalg.svd.SVD

val SVD(u, s, vt) = svd(A)
u
s
vt

// constants

import breeze.numerics.{NaN, Inf}
import breeze.numerics.constants.{Pi,e}
NaN
Inf
-Inf
Pi
e






