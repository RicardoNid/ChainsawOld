package DSP

import java.security._
import javax.crypto._

class RSA {


}

object RSA {
  def main(args: Array[String]): Unit = {
    val keys = KeyPairGenerator.getInstance("RSA").generateKeyPair()
    // you can find modulus and two exponents in the formatted string, but how to get them?
    println(keys.getPrivate)
    println(keys.getPublic)

    val text = "a" * 1000
    val cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, keys.getPublic)
    val result: Array[Byte] = cipher.doFinal()
    println(result.map(_.toBinaryString).mkString(""))

  }
}


