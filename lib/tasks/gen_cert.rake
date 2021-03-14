require "securerandom"

CERT_PATH = "config/gen_cert.pem"
PUB_KEY_PATH = "config/gen_key.pub.pem"
PRIV_KEY_PATH = "config/gen_key.pem"

def sign_new_cert(hostname)
  #lifted from the ruby docs
  key = OpenSSL::PKey::RSA.new 2048
  cert = OpenSSL::X509::Certificate.new
  cert.version = 2
  cert.serial = SecureRandom.random_number(2 ** 32)
  cert.subject = OpenSSL::X509::Name.parse "/CN=#{hostname}/O=#{hostname}/OU=#{hostname}"
  cert.issuer = cert.subject
  cert.public_key = key.public_key
  cert.not_before = Time.now
  cert.not_after = cert.not_before + 2 * 365 * 24 * 60 * 60
  ef = OpenSSL::X509::ExtensionFactory.new
  ef.subject_certificate = cert
  cert.add_extension(ef.create_extension("subjectKeyIdentifier", "hash", false))
  cert.sign(key, OpenSSL::Digest::SHA256.new)
  [key, cert]
end

desc "Generate ssl cert and private key."
task :gen_cert, [:hostname] => :environment do |t, args|
  hostname = (args.hostname or "localhost")
  key_and_cert = sign_new_cert(hostname)

  key = key_and_cert[0]
  File.open(PUB_KEY_PATH, "wb", 0600) do |f|
    f.write(key.public_key.to_pem)
  end
  File.open(PRIV_KEY_PATH, "wb", 0600) do |f|
    f.write(key.to_pem)
  end
  cert = key_and_cert[1]
  File.open(CERT_PATH, "wb", 0600) do |f|
    f.write(cert.to_pem)
  end
end
