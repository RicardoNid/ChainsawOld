package Chainsaw.Crypto.RSA

import sun.security.rsa._

import java.security._
import javax.crypto._

class Refs(lN: Int) {
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