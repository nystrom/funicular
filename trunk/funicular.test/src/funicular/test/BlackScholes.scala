package funicular.test

import funicular.Intrinsics._
import scala.util.Random

/*
 * This sample evaluates fair call and put prices for a
 * given set of European options by Black-Scholes formula.
 *
 * This program is inteneded to run on NVIDIA GPU accelerators
 */

object BlackScholes {

    def doBlackScholes(
            optionYears:Array[Float],
            stockPrice:Array[Float],
            optionStrike:Array[Float],
            callResult:Array[Float],
            putResult:Array[Float],
            opt_N:Int,
            R:Float,
            V:Float): Unit = {
        val blocks = 480
        val threads = 128
        //val blocks = CUDAUtilities.autoBlocks,
        //    threads = CUDAUtilities.autoThreads
        for (block <- 0 until blocks) {
            for (thread <- 0 until threads) async {
                val tid = block * threads + thread
                val tids = blocks * threads
                for (opt <- tid until opt_N by tids) {
                    // Constants for Polynomial approximation of cumulative normal distribution
                    val A1 = 0.31938153f
                    val A2 = -0.356563782f
                    val A3 = 1.781477937f
                    val A4 = -1.821255978f
                    val A5 = 1.330274429f
                    val RSQRT2PI = 0.39894228040143267793994605993438f

                    val T = optionYears(opt)
                    val S = stockPrice(opt)
                    val X = optionStrike(opt)
                    val sqrtT = Math.sqrt(T)
                    val d1 = (Math.log(S/X) + (R + 0.5f * V * V) * T) / (V * sqrtT)
                    val d2 = d1 - V * sqrtT

                    val K1 = 1.0f / (1.0f + 0.2316419f * Math.abs(d1))
                    val K2 = 1.0f / (1.0f + 0.2316419f * Math.abs(d2))
                    var CNDD1:Float = (RSQRT2PI * Math.exp(- 0.5f * d1 * d1) *
                        (K1 * (A1 + K1 * (A2 + K1 * (A3 + K1 * (A4 + K1 * A5)))))).toFloat
                    var CNDD2:Float = (RSQRT2PI * Math.exp(- 0.5f * d2 * d2) * 
                        (K2 * (A1 + K2 * (A2 + K2 * (A3 + K2 * (A4 + K2 * A5)))))).toFloat

                    if(d1 > 0) CNDD1 = 1.0f - CNDD1
                    if(d2 > 0) CNDD2 = 1.0f - CNDD2

                    //Calculate Call and Put simultaneously
                    val expRT = Math.exp(- R * T)
                    callResult(opt) = (S * CNDD1 - X * expRT * CNDD2).toFloat
                    putResult(opt)  = (X * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1)).toFloat
                }
            }
        }
    }

    def main (args: Array[String]) = finish {

        // Problem parameters
        val OPT_N = 4000000
        val NUM_ITERATIONS = 512
        val RISKFREE = 0.02f
        val VOLATILITY = 0.30f

        val rand = new Random

        // Host arrays
        val h_CallResultCPU = Array.make[Float](OPT_N, 0.0f)
        val h_PutResultCPU  = Array.make[Float](OPT_N, -1.0f)
        val h_CallResultGPU = Array.make[Float](OPT_N, 0.0f)
        val h_PutResultGPU  = Array.make[Float](OPT_N, 0.0f)
        val h_StockPrice    = Array.tabulate[Float](OPT_N)((i:Int)=>rand.nextFloat)
        val h_OptionStrike  = Array.tabulate[Float](OPT_N)((i:Int)=>rand.nextFloat)
        val h_OptionYears   = Array.tabulate[Float](OPT_N)((i:Int)=>rand.nextFloat)

        // Device arrays
        val d_CallResult    = Array.make[Float](OPT_N, 0.0f)
        val d_PutResult     = Array.make[Float](OPT_N, 0.0f)
        val d_StockPrice    = h_StockPrice.copy
        val d_OptionStrike  = h_OptionStrike.copy
        val d_OptionYears   = h_OptionYears.copy

        val gpuTimeStart = System.nanoTime
        for (i <- 0 until NUM_ITERATIONS) {
            doBlackScholes(
                    d_OptionYears,
                    d_StockPrice,
                    d_OptionStrike,
                    d_CallResult,
                    d_PutResult,
                    OPT_N,
                    RISKFREE,
                    VOLATILITY)
        }
        var gpuTime:Float = System.nanoTime - gpuTimeStart
        gpuTime /= NUM_ITERATIONS.toFloat
        println("Options count             : " + 2 * OPT_N)
        println("BlackScholesGPU time      : " + gpuTime/(1.0e-6f) + " msec")
        println("Effective memory bandwidth: " + (5 * OPT_N * 4) * 1.0e-9f / (gpuTime * 1.0E-9f) + " GB/s")
        println("Gigaoptions per second    : " + ((2 * OPT_N) * 1.0e-9f) / (gpuTime * 1.0e-9f))

        // Read back GPU results
        finish {
            Array.copy(d_CallResult, 0, h_CallResultGPU, 0, OPT_N)
            Array.copy(d_PutResult, 0, h_PutResultGPU, 0, OPT_N)
        }
        // Run BlackScholes on CPU to test results against
        doBlackScholes(
                h_OptionYears,
                h_StockPrice,
                h_OptionStrike,
                h_CallResultCPU,
                h_PutResultCPU,
                OPT_N,
                RISKFREE,
                VOLATILITY)
        // Check results
        var sum_delta:Float = 0.0f
        var sum_ref:Float = 0.0f
        var max_delta:Float = 0.0f
        for (i <- 0 until OPT_N) {
            val ref_val = h_CallResultCPU(i)
            val delta = Math.abs(ref_val - h_CallResultGPU(i))
            if(delta > max_delta) max_delta = delta
            sum_delta += delta
            sum_ref   += Math.abs(ref_val)
        }
        val L1norm = sum_delta / sum_ref
        println("L1 norm: " + L1norm)
        println("Max absolute error: " + max_delta)
        println(if (L1norm < 1e-6f) "TEST PASSED" else "TEST FAILED")
    }
}
