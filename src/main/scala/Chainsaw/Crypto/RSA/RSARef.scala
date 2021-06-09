package Chainsaw.Crypto.RSA

import sun.security.rsa._

import java.security._
import javax.crypto._

class RSARef(lN: Int) {
  require(Array(512, 1024, 2048, 3072, 4096).contains(lN))
  val keyGen = KeyPairGenerator.getInstance("RSA")
  keyGen.initialize(lN, new SecureRandom())

  var keyPair = keyGen.generateKeyPair()
  def refresh() = keyPair = keyGen.generateKeyPair()

  def getPrivate = keyPair.getPrivate
  def getPublic = keyPair.getPublic

  def privateImpl = keyPair.getPrivate.asInstanceOf[RSAPrivateCrtKeyImpl]

  def getModulus = privateImpl.getModulus // N

  def getPrivateValue = privateImpl.getPrivateExponent
  def getPublicValue = privateImpl.getPublicExponent

  def getP = privateImpl.getPrimeExponentP
  def getQ = privateImpl.getPrimeExponentQ

  def encode(message: Array[Byte], publicKey: PublicKey) = {
    val encoder = Cipher.getInstance("RSA")
    encoder.init(Cipher.ENCRYPT_MODE, publicKey)
    encoder.doFinal(message)
  }

  def decode(cipher: Array[Byte], privateKey: PrivateKey) = {
    val decoder = Cipher.getInstance("RSA")
    decoder.init(Cipher.DECRYPT_MODE, privateKey)
    decoder.doFinal(cipher)
  }
}


object RSARef {

  val tmep = 42.toByte

  def main(args: Array[String]): Unit = {

    val ref = new RSARef(512)
    val privateKey = ref.getPrivate
    val publicKey = ref.getPublic
    val cipher = ref.encode(("a" * 64).getBytes(), publicKey)
    val recovered = ref.decode(cipher, privateKey)
    println(recovered.map(_.toChar).mkString(""))


    //    val keys = KeyPairGenerator.getInstance("RSA").generateKeyPair()
    //    val keyGen = KeyPairGenerator.getInstance("RSA")
    //    keyGen.initialize(512, new SecureRandom())
    //    keyGen.generateKeyPair()
    //    keyGen.generateKeyPair()
    //    val keys = keyGen.generateKeyPair()
    //    val privateKeyValue = BigInt(keys.getPrivate.getEncoded)
    //    println(s"private key value $privateKeyValue")
    //
    //    val privateKey = keys.getPrivate.asInstanceOf[RSAPrivateCrtKeyImpl]
    //    // private key holds everything of public keys
    //    println(privateKey.getModulus) // n
    //    println(privateKey.getPrimeP) // p
    //    println(privateKey.getPrimeQ) // q
    //    println(privateKey.getPrivateExponent) // d
    //    println(s"private key value ${privateKey.getPrivateExponent}")
    //    println(privateKey.getPublicExponent) // e
    //    println(privateKey.getModulus.toString(2).size)
    //
    //    val publicKey = keys.getPublic.asInstanceOf[RSAPublicKeyImpl]
    //    println(publicKey.getPublicExponent)
    //
    //    val encoder = Cipher.getInstance("RSA")
    //    encoder.init(Cipher.ENCRYPT_MODE, keys.getPublic)
    //    val message = "a" * 53
    //    val cipher = encoder.doFinal(message.getBytes)
    //
    //    val decoder = Cipher.getInstance("RSA")
    //    decoder.init(Cipher.DECRYPT_MODE, keys.getPrivate)
    //    val rebuilt = decoder.doFinal(cipher)
    //    val rebuiltString = rebuilt.map(_.toChar).mkString("")
    //    println(rebuiltString)
    //    println(rebuiltString.size)
  }
}


