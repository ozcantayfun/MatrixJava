import java.util.Random;

final public class Matrix implements Comparable<Matrix>, Cloneable {
	private final int M;
	private final int N;
	private final double[][] matrixData;

	
	public Matrix(int M, int N) {
		this.M = M;
		this.N = N;
		matrixData = new double[M][N];
	}

	
	public Matrix(double[][] matrixData) {
		this.M = matrixData.length;
		this.N = matrixData[0].length;
		this.matrixData = new double[M][N];
		for (int row = 0; row < M; row++)
			for (int col = 0; col < N; col++)
				this.matrixData[row][col] = matrixData[row][col];
	}

	
	private Matrix(Matrix firstMatrix) {
		this(firstMatrix.matrixData);
	}

	
	public static Matrix random(int M, int N) {
		Matrix firstMatrix = new Matrix(M, N);
		Random random = new Random();
		for (int row = 0; row < M; row++) {
			for (int col = 0; col < N; col++) {
				firstMatrix.matrixData[row][col] = random.nextDouble();
			}
		}
		return firstMatrix;
	}

	
	public Matrix identity(int N) {
		Matrix temp = new Matrix(N, N);
		for (int row = 0; row < N; row++) {
			temp.matrixData[row][row] = 1;
		}
		return temp;
	}

	public double trace() {
		double sumOfDiagonal = 0;
		for (int row = 0; row < this.matrixData.length; row++) {
			sumOfDiagonal = (sumOfDiagonal + this.matrixData[row][row]);
		}
		return sumOfDiagonal;
	}

	public double frobenius() {
		double frobeniusNorm = 0;
		for (int row = 0; row < M; row++) {
			for (int col = 0; col < N; col++) {
				frobeniusNorm += this.matrixData[row][col];
			}
		}
		return Math.sqrt(frobeniusNorm);
	}

	public boolean isSymmetric() {
		for (int row = 0; row < M; row++) {
			for (int col = 0; col < N; col++) {
				if (this.matrixData[row][col] != this.matrixData[col][row]) {
					return false;
				}
			}
		}
		return true;
	}

	
	public void cofficient(Matrix firstMatrix, double temp[][], int p, int q, int n) {
		int k = 0, l = 0;
		for (int row = 0; row < n; row++) {
			for (int col = 0; col < n; col++) {
				if (row != p && col != q) {
					temp[k][l++] = firstMatrix.matrixData[row][col];
					if (l == n - 1) {
						l = 0;
						k++;
					}
				}
			}
		}
	}

	public Matrix adjoint() {
		Matrix adj;
		if (N == 1) {
			adj = new Matrix(1, 1);
			adj.matrixData[0][0] = 1;
			return adj;
		}
		int sign = 1;
		double[][] tempData = new double[N][N];
		adj = new Matrix(N, N);
		for (int row = 0; row < N; row++) {
			for (int col = 0; col < N; col++) {
				cofficient(this, tempData, row, col, N);
				sign = ((row + col) % 2 == 0) ? 1 : -1;
				adj.matrixData[col][row] = (sign) * (determinant(tempData));
			}
		}
		return adj;
	}

	public Matrix inverse() {
		Matrix inv;
		double det = determinant();
		if (det == 0) {
			return this;
		}
		inv = new Matrix(N, N);
		double[][] adj = adjoint().matrixData;
		for (int row = 0; row < N; row++)
			for (int col = 0; col < N; col++)
				inv.matrixData[row][col] = adj[row][col] / (float) det;
		return inv;
	}

	public Matrix transpose() {
		Matrix firstMatrix = new Matrix(N, M);
		for (int row = 0; row < M; row++)
			for (int col = 0; col < N; col++)
				firstMatrix.matrixData[col][row] = this.matrixData[row][col];
		return firstMatrix;
	}

	public Matrix plus(Matrix secondMatrix) {
		Matrix firstMatrix = this;
		if (firstMatrix.N != secondMatrix.M) {
			return this;
		}
		Matrix C = new Matrix(M, N);
		for (int row = 0; row < M; row++)
			for (int col = 0; col < N; col++)
				C.matrixData[row][col] = firstMatrix.matrixData[row][col] + secondMatrix.matrixData[row][col];
		return C;
	}

	public Matrix minus(Matrix secondMatrix) {
		Matrix firstMatrix = this;
		if (firstMatrix.N != secondMatrix.M) {
			return this;
		}
		Matrix C = new Matrix(M, N);
		for (int row = 0; row < M; row++)
			for (int col = 0; col < N; col++)
				C.matrixData[row][col] = firstMatrix.matrixData[row][col] - secondMatrix.matrixData[row][col];
		return C;
	}

	public boolean eq(Matrix secondMatrix) {
		Matrix firstMatrix = this;
		if (firstMatrix.N != secondMatrix.M) {
			return false;
		}
		for (int row = 0; row < M; row++)
			for (int col = 0; col < N; col++)
				if (firstMatrix.matrixData[row][col] != secondMatrix.matrixData[row][col])
					return false;
		return true;
	}

	public Matrix times(Matrix secondMatrix) {
		Matrix firstMatrix = this;
		if (firstMatrix.N != secondMatrix.M) {
			return this;
		}
		Matrix C = new Matrix(firstMatrix.M, secondMatrix.N);
		for (int row = 0; row < C.M; row++)
			for (int col = 0; col < C.N; col++)
				for (int k = 0; k < firstMatrix.N; k++)
					C.matrixData[row][col] += (firstMatrix.matrixData[row][k] * secondMatrix.matrixData[k][col]);
		return C;
	}

	public Matrix times(double scalar) {
		Matrix firstMatrix = this;
		for (int row = 0; row < firstMatrix.M; row++)
			for (int col = 0; col < firstMatrix.N; col++)
				for (int k = 0; k < firstMatrix.N; k++)
					firstMatrix.matrixData[row][col] += (firstMatrix.matrixData[row][k] * scalar);
		return firstMatrix;
	}

	public boolean isReversible() {
		return (determinant() != 0) ? true : false;
	}

	public double determinant() {
		return determinant(this.matrixData);
	}

	
	public double determinant(double[][] matrix) {
		double temporary[][];
		double result = 0;

		if (matrix.length == 1) {
			result = matrix[0][0];
			return (result);
		}

		if (matrix.length == 2) {
			result = ((matrix[0][0] * matrix[1][1]) - (matrix[0][1] * matrix[1][0]));
			return (result);
		}

		for (int row = 0; row < matrix[0].length; row++) {
			temporary = new double[matrix.length - 1][matrix[0].length - 1];

			for (int col = 1; col < matrix.length; col++) {
				for (int k = 0; k < matrix[0].length; k++) {
					if (k < row) {
						temporary[col - 1][k] = matrix[col][k];
					} else if (k > row) {
						temporary[col - 1][k - 1] = matrix[col][k];
					}
				}
			}

			result += matrix[0][row] * Math.pow(-1, (double) row) * determinant(temporary);
		}
		return (result);
	}

	
	private void swap(int row, int col) {
		double[] array = matrixData[row];
		matrixData[row] = matrixData[col];
		matrixData[col] = array;
	}

	
	public Matrix soln(Matrix rhs) {
		if (M != N || rhs.M != N || rhs.N != 1) {
			return this;
		}

		Matrix firstMatrix = new Matrix(this);
		Matrix secondMatrix = new Matrix(rhs);

		for (int row = 0; row < N; row++) {
			int max = row;
			for (int col = row + 1; col < N; col++)
				if (Math.abs(firstMatrix.matrixData[col][row]) > Math.abs(firstMatrix.matrixData[max][row]))
					max = col;
			firstMatrix.swap(row, max);
			secondMatrix.swap(row, max);

			for (int col = row + 1; col < N; col++)
				secondMatrix.matrixData[col][0] -= secondMatrix.matrixData[row][0] * firstMatrix.matrixData[col][row]
						/ firstMatrix.matrixData[row][row];

			for (int col = row + 1; col < N; col++) {
				double m = firstMatrix.matrixData[col][row] / firstMatrix.matrixData[row][row];
				for (int k = row + 1; k < N; k++) {
					firstMatrix.matrixData[col][k] -= firstMatrix.matrixData[row][k] * m;
				}
				firstMatrix.matrixData[col][row] = 0.0;
			}
		}

		Matrix x = new Matrix(N, 1);
		for (int col = N - 1; col >= 0; col--) {
			double transpose = 0.0;
			for (int k = col + 1; k < N; k++) {
				transpose += firstMatrix.matrixData[col][k] * x.matrixData[k][0];
			}
			x.matrixData[col][0] = (secondMatrix.matrixData[col][0] - transpose) / firstMatrix.matrixData[col][col];
		}
		return x;

	}

	public void show() {
		for (int row = 0; row < M; row++) {
			for (int col = 0; col < N; col++) {
				System.out.printf("%8.4f ", matrixData[row][col]);
			}
			System.out.println();
		}
	}

	@Override
	public int compareTo(Matrix o) {
		for (int row = 0; row < M; row++) {
			for (int col = 0; col < N; col++) {
				if (this.matrixData[row][col] != o.matrixData[row][col]) {
					return -1;
				}
			}
		}
		return 0;
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		double[][] mat = { { 1.0000,2.0000,3.0000}, { 4.0000,5.0000,6.0000}, { 9.0000, 1.0000, 3.0000 } };		
		Matrix T = new Matrix(mat);
		System.out.println("Orjinal Matrix by Manuel : ");
		T.show();
		Matrix O = Matrix.random(3, 3);
		System.out.println("Matrix by Random : ");
		O.show();
		System.out.println("is it Reversible? "+T.isReversible());
        System.out.println("The determinat of the manuel matrix is "+T.determinant());
        System.out.println("The swap of the manuel matrix is ");
        T.swap(1,2);
        T.show();
		System.out.println("identity : ");
		Matrix A= T.identity(3);
		A.show();
		System.out.println("trace : "+T.trace());
		System.out.println("frobenius : "+T.frobenius());
		System.out.println("isSymmeric : "+T.isSymmetric());		
		System.out.println("Transpose Matrix by Manuel : ");
		Matrix Y= T.transpose();
		Y.show();
		System.out.println("Matrix T plus O  : ");
		Matrix F= T.plus(O);
		F.show();
		System.out.println("Matrix T minus O  : ");
		Matrix U= T.minus(O);
		U.show();
		System.out.println("Matrix T times O  : ");
		Matrix N= T.times(O);
		N.show();
		System.out.println("Matrix T times with any scalar : ");
		Matrix o= T.times(2);
		o.show();	

        System.out.println("Matrix soln : ");
		Matrix z= T.soln(O);
		z.show();
		System.out.println("Is T Compare to O? "+T.compareTo(T));
		System.out.println("Is T eq to T? "+T.eq(O));	
		System.out.println("transpose matrixxxxx : ");
		Matrix g= T.transpose();
		g.show();
		
		

	}

}
