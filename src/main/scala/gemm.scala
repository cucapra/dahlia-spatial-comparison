import spatial.dsl._

@spatial object GEMM_NCubed_all extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 1 (1->1->16)
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   


@spatial object GEMM_NCubed_1 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 1
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_2 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 2
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   
@spatial object GEMM_NCubed_3 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 3
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   



@spatial object GEMM_NCubed_4 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 4
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_5 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 5
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_6 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 6
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   
@spatial object GEMM_NCubed_7 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 7
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   



@spatial object GEMM_NCubed_8 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 8
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_9 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 9
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   



@spatial object GEMM_NCubed_10 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 10
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   




@spatial object GEMM_NCubed_11 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 11
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_12 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 12
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   
@spatial object GEMM_NCubed_13 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 13
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   



@spatial object GEMM_NCubed_14 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 14
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_15 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 15
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_NCubed_16 extends SpatialApp {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val dim = 128
    val loop_k = 16
    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)

    Accel{
      val a_sram = SRAM[T](dim,dim)
      val b_sram = SRAM[T](dim,dim)
      val c_sram = SRAM[T](dim,dim)

      a_sram load a_dram
      b_sram load b_dram

      Foreach(dim by 1) { i => 
        Foreach(dim by 1) { j => 
          val sum = Reduce(Reg[T](0))(dim by 1 par loop_k) { k => 
            a_sram(i,k) * b_sram(k,j)
          }{_+_}
          c_sram(i,j) = sum
        }
      }
      c_dram store c_sram
    }

    //val c_gold = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_NCubed)")
    assert(cksum)
  }
}   

@spatial object GEMM_Blocked extends SpatialApp { // Regression (Dense) // Args: 128
  //override def dseModelArgs: Args = "128 128 128"
  //override def finalModelArgs: Args = "128 128 128"
  //override def runtimeArgs: Args = "128"   
  type T = FixPt[TRUE,_16,_16] // Fatter type so that tileSize is burst aligned


  def main(args: Array[String]): Unit = {

    //val dim_arg = args(0).to[Int]
    val dim = 128 //ArgIn[Int]
    //setArg(dim, dim_arg)
    val tileSize = 16 (16 -> 16 -> 128)
    val i_tileSize = 64 (64 -> 16 -> 128)
    val par_load = 1
    val par_store = 1
    val loop_jj    = 1 // (1 -> 1 -> dim/tileSize) // THIS PAR DOES NOT WORK UNTIL BUG #205 IS FIXED
    val loop_ii    = 1 // not sure if this one works
    val loop_kk    = 1 //(1 -> 1 -> 8)
    val loop_i     = 1 //(1 -> 1 -> 32)
    val loop_k     = 1 //(1 -> 1 -> 16)
    val loop_j     = 1 //(1 -> 1 -> 16)
    val reduce_col = 1 //(1 -> 1 -> 16)
    val reduce_tmp = 1 //(1 -> 1 -> 16)

    //val a_data = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_a.csv", "\n").reshape(dim,dim)
    //val b_data = loadCSV1D[T]( sys.env("DATA_HOME") + "gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val b_data = (0::dim,0::dim){(i,j) => random[T](5)}
    val c_init = (0::dim, 0::dim){(i,j) => 0.to[T]}
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{

      Foreach(dim by i_tileSize par loop_ii) { ii => // this loop defenitilely cant be parallelized right now
        Foreach(dim by tileSize par loop_jj) { jj => 
          val c_col = SRAM[T](i_tileSize,tileSize)
          MemReduce(c_col(0::i_tileSize, 0::tileSize par reduce_col))(dim by tileSize par loop_kk) { kk => 
            val c_col_partial = SRAM[T](i_tileSize,tileSize)
            val b_sram = SRAM[T](tileSize,tileSize)
            b_sram load b_dram(kk::kk.to[I32]+tileSize, jj::jj.to[I32]+tileSize par par_load)
            Foreach(i_tileSize by 1 par loop_i) { i => 
              val a_sram = SRAM[T](tileSize)
              a_sram load a_dram(ii+i, kk::kk.to[I32]+tileSize)
              val c_tmp = SRAM[T](tileSize)
              MemReduce(c_tmp par reduce_tmp)(tileSize by 1 par loop_k) { k => 
                val c_tmp_partial = SRAM[T](tileSize)
                val temp_a = a_sram(k)
                Foreach(tileSize by 1 par loop_j) { j => 
                  c_tmp_partial(j) = b_sram(k, j) * temp_a
                }
                c_tmp_partial
              }{_+_}
            Foreach(tileSize by 1){cpy => c_col_partial(i,cpy) = c_tmp(cpy)}
            }
          c_col_partial
          }{_+_}
          c_dram(ii::ii.to[I32]+i_tileSize, jj::jj.to[I32]+tileSize par par_store) store c_col
        }
      }
    }

    // val c_gold = loadCSV1D[T](s"$DATA/gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim,0::dim){(i,j) => 
      Array.tabulate(dim){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
    assert(cksum)
  }
}

