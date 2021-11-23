package Chainsaw.crypto.classicMceliece

object operations {

  type uChar=Char

  def kemKeyPair:(Array[uChar],Array[uChar])={

    var seedRand=new Array[uChar](32)
    val seedHead=new Array[uChar](1)
    var r=new Array[uChar](SYS_N/8+(GFBITS<<1)*4+SYS_T*2+32)



    //generate random seed
    seedRand=randomBytes(32)
    seedHead(0)=64.toChar
    var seedFull=Array.concat(seedHead,seedRand)
    var pk=new Array[uChar](1) //todo:match the size of pk and sk
    var sk=new Array[uChar](1)

    while (true){
      // generate full r and update seed
      seedFull=Array.concat(seedHead,seedRand)
      r=shake(seedFull,33,r.length)
      // update sk4 in sk, seed before update
      var sk1=seedRand // also need 8 uchar in tail
      // depart r into four parts:s,perm,f,seed
      // update seed , r4 in r
      seedRand=r.drop(SYS_N/8+(GFBITS<<1)*4+SYS_T*2)
      // initial f ,r3 in r
      var f =r.drop(SYS_N/8+(GFBITS<<1)*4).dropRight(32)
      // initial perm
      var perm =r.drop(SYS_N/8).dropRight(SYS_T*2+32)

      // generate irr ,sk2 in sk
      var (sk2,irrSucc) = genpolyGen(f)
      //TODO: if irrSucc is false,continue

      // generate permutation
      var (pktmp,pi,pkSucc)=pkGen(sk2,perm)
      //TODO:if pkSucc is false,continue

      // generate COND_BYTES,sk3 in sk
      var sk3=controlBits(pi,GFBITS,GFBITS<<1)

      // initial r1 in r as sk4
      var sk4 =r.dropRight((GFBITS<<1)*4+SYS_T*2+32)

      var sk18=new Array[uChar](8)
      (0 until(sk18.length)).foreach(i=> sk18(i)=0xFFFF.toChar)
      var sktmp=Array.concat(sk1,sk18,sk2,sk3,sk4)
      pk=pktmp
      sk=sktmp

    }
    (pk,sk)

  }


  def kemEnc(pk:Array[uChar]):(Array[uChar],Array[uChar])={

    // generate erand and c
    var (c,erand)=encrypt(pk)
    val eHead=new Array[uChar](1);eHead(0)=2.toChar
    var eFull=Array.concat(eHead,erand)
    var syndBytes=shake(eFull,eFull.length,32)

    val ecHead=new Array[uChar](1)
    ecHead(0)=1.toChar
    var ec=Array.concat(ecHead,erand,c,syndBytes)
    var key=shake(ec,ec.size,32)

    (c,key)

  }


  def kemDec(sk:Array[uChar],c:Array[uChar]):Array[uChar]={

    val irrTail = sk.drop(40)
    val s=sk.drop(40+IRR_BYTES+COND_BYTES)
    var (retDec,e)=decrypt(irrTail,c)
    var eHead=new Array[uChar](1)
    eHead(0)=2.toChar
    var eFull=Array.concat(eHead,e)

    var conf=shake(eFull,eFull.size,32)

    var retConf:uChar=0.toChar

    (0 until(32)).foreach{i =>
      retConf |= conf(i)^c(SYND_BYTES+i)
    }

    var m:gfUint16=(retDec | retConf ).toShort// m is 16 bit uint
    m -=1
    m >>=8

    //generate preimage
    val preHead=new Array[uChar](1)
    preHead(0)=(m&1).toChar
    var pre2=new Array[uChar](SYS_N/8)
    (0 until(SYS_N/8)).foreach{i =>
      pre2(i)=((~m & s(i))|(m & e(i))).toChar
    }
    val preImage=Array.concat(preHead,pre2,c)
    val key= shake(preImage,preImage.size,32)

    key



  }




}
