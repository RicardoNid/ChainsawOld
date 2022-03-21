package Chainsaw.algos

import sun.security.rsa._

import java.security._
import javax.crypto._
import scala.collection.mutable.ArrayBuffer

case class RsaParams(lN: Int) {

  require(Array(512, 1024, 2048, 3072, 4096).contains(lN))
  val keyGen = KeyPairGenerator.getInstance("RSA")
  keyGen.initialize(lN, new SecureRandom())

  var keyPair = keyGen.generateKeyPair()

  def refresh() = keyPair = keyGen.generateKeyPair()

  def privateImpl = keyPair.getPrivate.asInstanceOf[RSAPrivateCrtKeyImpl]

  // P,Q are the srarting point of param gen
  def getP = BigInt(privateImpl.getPrimeExponentP)

  def getQ = BigInt(privateImpl.getPrimeExponentQ)

  // modulo N, N = P * Q
  def getModulus = BigInt(privateImpl.getModulus) // N

  // private key
  def getPrivateKey = BigInt(privateImpl.getPrivateExponent)

  // public key
  def getPublicKey = BigInt(privateImpl.getPublicExponent)

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

  // generate multiple groups of parameters
  def getParams(num: Int) = {
    val modulusBuffer    = ArrayBuffer[BigInt]()
    val publicKeyBuffer  = ArrayBuffer[BigInt]()
    val privateKeyBuffer = ArrayBuffer[BigInt]()

    (0 until num).foreach { _ =>
      modulusBuffer += getModulus
      publicKeyBuffer += getPublicKey
      privateKeyBuffer += getPrivateKey
      refresh()
    }

    (modulusBuffer, publicKeyBuffer, privateKeyBuffer)
  }
}
