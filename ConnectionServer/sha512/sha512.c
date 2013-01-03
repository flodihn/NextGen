#include <openssl/evp.h> 
#include <string.h>

unsigned char *sha512(char *pText) 
{
    EVP_MD_CTX mdctx;
    const EVP_MD *md;
    unsigned char *md_value;
    unsigned int md_len;
    int i;

    md_value = malloc(EVP_MAX_MD_SIZE);
    OpenSSL_add_all_digests();
    md = EVP_sha512();
    if(!md) {
        return "";
    }   
    EVP_MD_CTX_init(&mdctx);
    EVP_DigestInit_ex(&mdctx, md, NULL);
    EVP_DigestUpdate(&mdctx, pText, strlen(pText));
    EVP_DigestFinal_ex(&mdctx, md_value, &md_len);
    EVP_MD_CTX_cleanup(&mdctx);
    
    /*
    for(i = 0; i < md_len; i++) fprintf(stderr, "%02x", md_value[i]);
        fprintf(stderr, "\n");
    */
    return md_value; 
}

