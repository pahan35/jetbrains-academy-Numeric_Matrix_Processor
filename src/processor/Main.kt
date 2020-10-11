package processor

import java.util.*
import kotlin.system.exitProcess

class IncorrectSizeException : Exception()

class IncorrectOperationException : Exception()

class InversionDoesntExistException : Exception()

enum class Transpose(val number: Int, val label: String) {
    MAIN_DIAGONAL(1, "Main diagonal"),
    SIDE_DIAGONAL(2, "Side diagonal"),
    VERTICAL_LINE(3, "Vertical line"),
    HORIZONTAL_LINE(4, "Horizontal line");

    companion object {
        fun findByInput(number: Int): Transpose? {
            return values().find { it.number == number }
        }
    }
}

abstract class AbstractMatrixContainer<T>(var rawList: List<List<T>>) {
    val rows: Int
        get() = rawList.size
    val cols: Int
        get() = rawList[0].size
    val size: Pair<Int, Int>
        get() = Pair(rows, cols)

    abstract val zero: T
    abstract val one: T
    abstract val minusOne: T
    abstract fun plusCells(a: T, b: T): T
    abstract fun minusCell(a: T, b: T): T
    abstract fun multiplyCell(a: T, b: T): T
    abstract fun divideCell(a: T, b: T): T
}

class IntMatrixContainer(rawList: List<List<Int>>): AbstractMatrixContainer<Int>(rawList) {
    override val zero = 0
    override val one = 1
    override val minusOne = -1

    override fun plusCells(a: Int, b: Int) = a + b
    
    override fun minusCell(a: Int, b: Int) = a - b

    override fun multiplyCell(a: Int, b: Int) = a * b

    override fun divideCell(a: Int, b: Int) = a / b
}

class DoubleMatrixContainer(rawList: List<List<Double>>): AbstractMatrixContainer<Double>(rawList) {
    override val zero = 0.0
    override val one = 1.0
    override val minusOne = -1.0

    override fun plusCells(a: Double, b: Double) = a + b
    
    override fun minusCell(a: Double, b: Double) = a - b

    override fun multiplyCell(a: Double, b: Double) = a * b

    override fun divideCell(a: Double, b: Double) = a / b
}

abstract class AbstractMatrix<T>(private var matrixContainer: AbstractMatrixContainer<T>) where T : Number {
    private val rows: Int
        get() = matrixContainer.rows
    private val cols: Int
        get() = matrixContainer.cols
    private val size: Pair<Int, Int>
        get() = matrixContainer.size

    private val rawList: List<List<T>>
        get() = matrixContainer.rawList

    private val zero: T
        get() = matrixContainer.zero

    private val one: T
        get() = matrixContainer.one

    private val minusOne: T
        get() = matrixContainer.minusOne

    override fun toString(): String {
        return rawList.foldIndexed("") { rowIndex, resultAcc, column ->
            var rowString = column.foldIndexed("") { columnIndex, rowAcc, value ->
                var rowPart = rowAcc + value
                if (columnIndex != column.lastIndex) {
                    rowPart += " "
                }
                rowPart
            }
            if (rowIndex != rawList.lastIndex) {
                rowString += "\n"
            }
            resultAcc + rowString
        }
    }

    private fun plusCells(a: T, b: T) = matrixContainer.plusCells(a, b)

    private fun minusCell(a: T, b: T) = matrixContainer.minusCell(a, b)

    private fun multiplyCell(a: T, b: T)  = matrixContainer.multiplyCell(a, b)

    private fun divideCell(a: T, b: T) = matrixContainer.divideCell(a, b)

    private fun calcNewRawListOnMultiply(constant: T): List<List<T>> {
        return rawList.map { row -> row.map { multiplyCell(it, constant) } }
    }

    private fun iterateTranspose(currentRawList: List<List<T>>, fillValue: (Int, Int, T) -> Unit) {
        currentRawList.forEachIndexed { rowIndex, row ->
            row.forEachIndexed { columnIndex, item ->
                fillValue(rowIndex, columnIndex, item)
            }
        }
    }

    private fun multiplyRow(row: List<T>, anotherColumn: List<T>): T {
        return row.foldIndexed(zero) { rowItemIndex, acc, value -> plusCells(acc, multiplyCell(value, anotherColumn[rowItemIndex])) }
    }

    private fun cofactor(currentRawList: List<List<T>>, rowToGo: Int, columnIndex: Int): T {
        val tempRawList = mutableListOf<MutableList<T>>()
        var tempRawRow: Int
        var tempRawColumn: Int
        iterateTranspose(currentRawList) { rowIndex, internalColumnIndex, item ->
            if (rowIndex == rowToGo || columnIndex == internalColumnIndex) {
                return@iterateTranspose
            }
            tempRawRow = rowIndex + (if (rowIndex < rowToGo) 0 else -1)
            tempRawColumn = internalColumnIndex + (if (internalColumnIndex < columnIndex) 0 else -1)
            fillValue(tempRawList, tempRawRow, tempRawColumn, item)
        }
        val cofactorDeterminant = determinant(tempRawList)
        if (cofactorDeterminant == zero) {
            return cofactorDeterminant
        }
        val minusOneInPow = if ((rowToGo + 1 + columnIndex + 1) % 2 == 0) one else minusOne
        return multiplyCell(minusOneInPow, determinant(tempRawList))
    }

    fun determinant(currentRawList: List<List<T>> = rawList): T {
        if (currentRawList.size != currentRawList[0].size) {
            throw IncorrectSizeException()
        }
        if (currentRawList.size == 1) {
            return currentRawList[0][0]
        }
        if (currentRawList.size == 2) {
            return minusCell(
                    multiplyCell(currentRawList[0][0], currentRawList[1][1]),
                    multiplyCell(currentRawList[0][1], currentRawList[1][0])
            )
        }
        val rowToGo = 0
        return currentRawList[rowToGo].foldIndexed(zero) { columnIndex, acc, _ ->
            plusCells(acc, multiplyCell(currentRawList[rowToGo][columnIndex], cofactor(currentRawList, rowToGo, columnIndex)))
        }
    }

    private fun fillValue(targetRawList: MutableList<MutableList<T>>, targetRow: Int, targetColumn: Int, item: T) {
        while (targetRawList.size <= targetRow) {
            targetRawList.add(mutableListOf())
        }
        val rowToAdd = targetRawList[targetRow]
        while (rowToAdd.size <= targetColumn) {
            rowToAdd.add(item)
        }
        rowToAdd[targetColumn] = item
    }

    fun transpose(transpose: Transpose): AbstractMatrix<T> {
        val newRawList = mutableListOf<MutableList<T>>()

        fun fillValue(targetRow: Int, targetColumn: Int, item: T) = this.fillValue(newRawList, targetRow, targetColumn, item)

        fun iterateTranspose(handle: (Int, Int, T) -> Unit) = this.iterateTranspose(rawList, handle)

        when (transpose) {
            Transpose.MAIN_DIAGONAL -> {
                iterateTranspose { rowIndex, columnIndex, item -> fillValue(columnIndex, rowIndex, item) }
            }
            Transpose.SIDE_DIAGONAL -> {
                iterateTranspose { rowIndex, columnIndex, item ->
                    fillValue(
                            rawList.lastIndex - columnIndex,
                            rawList[rowIndex].lastIndex - rowIndex,
                            item
                    )
                }
            }
            Transpose.VERTICAL_LINE -> {
                iterateTranspose { rowIndex, columnIndex, item ->
                    fillValue(
                            rowIndex,
                            rawList[rowIndex].lastIndex - columnIndex,
                            item
                    )
                }
            }
            Transpose.HORIZONTAL_LINE -> {
                iterateTranspose { rowIndex, columnIndex, item ->
                    fillValue(
                            rawList.lastIndex - rowIndex,
                            columnIndex,
                            item
                    )
                }
            }
        }
        matrixContainer.rawList = newRawList.map { it.map { i -> i } }
        return this
    }

    operator fun plus(anotherMatrix: AbstractMatrix<T>): AbstractMatrix<T> {
        if (size != anotherMatrix.size) {
            throw IncorrectSizeException()
        }
        matrixContainer.rawList = rawList.mapIndexed { n, row ->
            row.mapIndexed { m, value ->
                plusCells(value, anotherMatrix.rawList[n][m])
            }
        }
        return this
    }

    operator fun times(constant: T): AbstractMatrix<T> {
        matrixContainer.rawList = calcNewRawListOnMultiply(constant)
        return this
    }

    operator fun times(matrix: AbstractMatrix<T>): AbstractMatrix<T> {
        if (cols != matrix.rows) {
            throw IncorrectSizeException()
        }
        matrixContainer.rawList = rawList.map { row ->
            List(matrix.cols) { i ->
                val anotherColumn = List(matrix.rows) { matrix.rawList[it][i] }
                multiplyRow(row, anotherColumn)
            }
        }
        return this
    }

    fun inverse(): AbstractMatrix<T> {
        val determinant = this.determinant()
        if (determinant == zero) {
            throw InversionDoesntExistException()
        }
        matrixContainer.rawList = rawList.mapIndexed { rowIndex, row ->
            row.mapIndexed { columnIndex, _ ->
                cofactor(rawList, rowIndex, columnIndex)
            }
        }
        transpose(Transpose.MAIN_DIAGONAL)
        return this * (divideCell(one, determinant))
    }
}

class DoubleMatrix(rawList: List<List<Double>>) : AbstractMatrix<Double>(DoubleMatrixContainer(rawList))

fun readMatrixInput(scanner: Scanner, matrixName: String? = null): Pair<Int, Int> {
    val matrixNamePart = (if (matrixName == null) "" else "$matrixName ") + "matrix"
    print("Enter size of $matrixNamePart: ")
    val rows = scanner.nextInt()
    val columns = scanner.nextInt()
    println("Enter $matrixNamePart: ")
    return Pair(rows, columns)
}

fun readIntMatrix(scanner: Scanner, matrixName: String? = null): List<List<Int>> {
    val (rows, columns) = readMatrixInput(scanner, matrixName)
    return List(rows) { List(columns) { scanner.nextInt() } }
}

fun readDoubleMatrix(scanner: Scanner, matrixName: String? = null): List<List<Double>> {
    val (rows, columns) = readMatrixInput(scanner, matrixName)
    return List(rows) { List(columns) { scanner.nextDouble() } }
}

enum class Action(val number: Int, val label: String) {
    ADD_MATRICES(1, "Add matrices"),
    MULTIPLY_BY_CONST(2, "Multiply matrix by a constant"),
    MULTIPLY_MATRICES(3, "Multiply matrices"),
    TRANSPOSE_MATRIX(4, "Transpose matrix"),
    CALCULATE_DETERMINANT(5, "Calculate a determinant"),
    INVERSE_MATRIX(6, "Inverse matrix"),
    EXIT(0, "Exit");

    companion object {
        fun findByInput(number: Int): Action? {
            return values().find { it.number == number }
        }
    }
}

fun printTransposes() {
    for (action in Transpose.values()) {
        println("${action.number}. ${action.label}")
    }
}

fun printMenu() {
    for (action in Action.values()) {
        println("${action.number}. ${action.label}")
    }
}

fun readTwoMatrix(scanner: Scanner): Pair<DoubleMatrix, DoubleMatrix> {
    val matrixA = DoubleMatrix(readDoubleMatrix(scanner, "first"))
    val matrixB = DoubleMatrix(readDoubleMatrix(scanner, "second"))

    return Pair(matrixA, matrixB)
}

fun main() {
    val scanner = Scanner(System.`in`).useLocale(Locale.US)

    while (true) {
        printMenu()
        print("Your choice: ")
        try {
            val result = when (Action.findByInput(scanner.nextInt()) ?: throw IncorrectOperationException()) {
                Action.ADD_MATRICES -> {
                    val (matrixA, matrixB) = readTwoMatrix(scanner)
                    (matrixA + matrixB).toString()
                }
                Action.MULTIPLY_BY_CONST -> {
                    val matrix = DoubleMatrix(readDoubleMatrix(scanner))
                    print("Enter constant: ")
                    val constant = scanner.nextDouble()
                    (matrix * constant).toString()
                }
                Action.MULTIPLY_MATRICES -> {
                    val (matrixA, matrixB) = readTwoMatrix(scanner)
                    (matrixA * matrixB).toString()
                }
                Action.TRANSPOSE_MATRIX -> {
                    printTransposes()
                    print("Your choice: ")
                    val transpose = Transpose.findByInput(scanner.nextInt()) ?: throw IncorrectOperationException()

                    val matrix = DoubleMatrix(readDoubleMatrix(scanner))

                    matrix.transpose(transpose).toString()
                }
                Action.CALCULATE_DETERMINANT -> {
                    val matrix = DoubleMatrix(readDoubleMatrix(scanner))
                    matrix.determinant().toString()
                }
                Action.INVERSE_MATRIX -> {
                    val matrix = DoubleMatrix(readDoubleMatrix(scanner))
                    matrix.inverse().toString()
                }
                Action.EXIT -> exitProcess(0)
            }
            println("The result is:")
            println(result)
        } catch (e: IncorrectSizeException) {
            println("The operation cannot be performed.")
        } catch (e: IncorrectOperationException) {
            println("Unknown operation.")
        } catch (e: InversionDoesntExistException) {
            println("This matrix doesn't have an inverse.")
        }
        println()
    }
}

