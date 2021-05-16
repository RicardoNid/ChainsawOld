package Chainsaw.Crypto

import collection.JavaConversions._

object AESGCM {
  val plainText = "This is a plain text which need to be encrypted by Java AES 256 GCM Encryption Algorithm"
  val AES_KEY_SIZE = 256
  val GCM_IV_LENGTH = 12 // bytes
  val GCM_TAG_LENGTH = 16 // bytes

  import javax.crypto.KeyGenerator
  import java.security.SecureRandom
  import java.util.Base64

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val keyGenerator = KeyGenerator.getInstance("AES")
    keyGenerator.init(AES_KEY_SIZE)
    // Generate Key
    val key = keyGenerator.generateKey
    val IV = new Array[Byte](GCM_IV_LENGTH)
    val random = new SecureRandom()
    random.nextBytes(IV)

    System.out.println("Original Text : " + plainText)
    val cipherText = encrypt(plainText.getBytes, key, IV)
    System.out.println("Encrypted Text : " + Base64.getEncoder.encodeToString(cipherText))
    val decryptedText = decrypt(cipherText, key, IV)
    System.out.println("DeCrypted Text : " + decryptedText)
  }

  import javax.crypto.Cipher
  import javax.crypto.SecretKey
  import javax.crypto.spec.GCMParameterSpec
  import javax.crypto.spec.SecretKeySpec

  @throws[Exception]
  def encrypt(plaintext: Array[Byte], key: SecretKey, IV: Array[Byte]) = { // Get Cipher Instance
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    // Create SecretKeySpec
    val keySpec = new SecretKeySpec(key.getEncoded, "AES")
    // Create GCMParameterSpec
    val gcmParameterSpec = new GCMParameterSpec(GCM_TAG_LENGTH * 8, IV)
    // Initialize Cipher for ENCRYPT_MODE
    cipher.init(Cipher.ENCRYPT_MODE, keySpec, gcmParameterSpec)
    // Perform Encryption
    val cipherText = cipher.doFinal(plaintext)
    cipherText
  }

  import javax.crypto.Cipher
  import javax.crypto.SecretKey
  import javax.crypto.spec.GCMParameterSpec
  import javax.crypto.spec.SecretKeySpec

  @throws[Exception]
  def decrypt(cipherText: Array[Byte], key: SecretKey, IV: Array[Byte]) = { // Get Cipher Instance
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    // Create SecretKeySpec
    val keySpec = new SecretKeySpec(key.getEncoded, "AES")
    // Create GCMParameterSpec
    val gcmParameterSpec = new GCMParameterSpec(GCM_TAG_LENGTH * 8, IV)
    // Initialize Cipher for DECRYPT_MODE
    cipher.init(Cipher.DECRYPT_MODE, keySpec, gcmParameterSpec)
    // Perform Decryption
    val decryptedText = cipher.doFinal(cipherText)
    new String(decryptedText)
  }
}
