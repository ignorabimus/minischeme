#ifndef BIGNUM_H
#define BIGNUM_H

#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

int64_t gcd(int32_t x, int32_t y);
int64_t lcm(int32_t x, int32_t y);
int32_t find1_32(uint32_t val);
int32_t bn_eq(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
int32_t bn_gt(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
int32_t bn_ge(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_add(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_sub(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_mul(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_sqr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx);
void bn_sftl(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n);
void bn_sftr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n);
void bn_div(uint32_t q[], int32_t *colq, uint32_t r[], int32_t *colr, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
int32_t bn_str2num_base2(const char s[], int32_t len, uint32_t x[], int32_t col);
int32_t bn_str2num_base8(const char s[], int32_t len, uint32_t x[], int32_t col);
int32_t bn_str2num_base10(const char s[], int32_t len, uint32_t x[], int32_t col);
int32_t bn_str2num_base16(const char s[], int32_t len, uint32_t x[], int32_t col);
char *bn_num2str_base2(char *p, uint32_t x[], int32_t col);
char *bn_num2str_base8(char *p, uint32_t x[], int32_t col);
char *bn_num2str_base10(char *p, uint32_t x[], int32_t col);
char *bn_num2str_base16(char *p, uint32_t x[], int32_t col);

#ifdef __cplusplus
}
#endif

#endif /* BIGNUM_H */
