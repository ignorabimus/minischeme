/*
 * This software is released under the MIT License, see the LICENSE file.
 */

#include "bignum.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* greatest common divisor */
int64_t gcd(int32_t x, int32_t y)
{
	uint32_t ux = (uint32_t)llabs(x), uy = (uint32_t)llabs(y), uz;
	while (ux != 0) {
		uz = ux;
		ux = uy % ux;
		uy = uz;
	}
	return uy;
}

/* least common multiple */
int64_t lcm(int32_t x, int32_t y)
{
	if (x == 0 || y == 0) {
		return 0;
	}
	return llabs(x / gcd(x, y) * y);
}

/* the first bit1 position */
int32_t find1_32(uint32_t val)
{
	static const int32_t clz_table_4bit[16] = { 4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 };
	int32_t n = 32;

	if ((val & 0xFFFF0000) == 0) { n -= 16; val <<= 16; }
	if ((val & 0xFF000000) == 0) { n -= 8; val <<= 8; }
	if ((val & 0xF0000000) == 0) { n -= 4; val <<= 4; }

	return n - clz_table_4bit[val >> (32 - 4)];
}

/* if x == y */
int32_t bn_eq(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (colx != coly) {
		return  0;
	}
	for (i = colx - 1; i >= 0; i--) {
		if (x[i] != y[i]) {
			return 0;
		}
	}
	return 1;
}

/* if x > y */
int32_t bn_gt(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (colx > coly) {
		return  1;
	} else if (colx < coly) {
		return 0;
	}
	for (i = colx - 1; i >= 0; i--) {
		if (x[i] > y[i]) {
			return 1;
		} else if (x[i] < y[i]) {
			return 0;
		}
	}
	return 0;
}

/* if x >= y */
int32_t bn_ge(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (colx > coly) {
		return  1;
	} else if (colx < coly) {
		return 0;
	}
	for (i = colx - 1; i >= 0; i--) {
		if (x[i] > y[i]) {
			return 1;
		} else if (x[i] < y[i]) {
			return 0;
		}
	}
	return 1;
}

/* z = x + y */
void bn_add(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i, col = (colx < coly) ? colx : coly;
	uint64_t t = 0;
	for (i = 0; i < col; i++) {
		t = (uint64_t)x[i] + y[i] + (t >> 32);
		z[i] = (uint32_t)t;
	}
	if (colx > coly) {
		*colz = colx;
		for (; i < colx; i++) {
			t = x[i] + (t >> 32);
			z[i] = (uint32_t)t;
		}
	} else {
		*colz = coly;
		for (; i < coly; i++) {
			t = y[i] + (t >> 32);
			z[i] = (uint32_t)t;
		}
	}
	if (t >>= 32) {
		z[(*colz)++] = (uint32_t)t;
	}
}

/* z = x - y */
void bn_sub(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	uint32_t carry = 0;
	for (i = 0; i < coly; i++) {
		uint64_t t = (uint64_t)y[i] + carry;
		if (x[i] >= t) {
			z[i] = (uint32_t)(x[i] - t);
			carry = 0;
		} else {
			z[i] = (uint32_t)(((uint64_t)1 << 32) + x[i] - t);
			carry = 1;
		}
	}
	*colz = colx;
	for (; i < colx; i++) {
		if (x[i] >= carry) {
			z[i] = (uint32_t)(x[i] - carry);
			carry = 0;
		} else {
			z[i] = (uint32_t)(((uint64_t)1 << 32) + x[i] - carry);
			carry = 1;
		}
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
}

/* z = x * y */
void bn_mul(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i, j;
	memset(z, 0, sizeof(uint32_t) * (colx + coly));
	for (i = 0; i < colx; i++) {
		uint64_t t = 0;
		for (j = 0; j < coly; j++) {
			t = z[i + j] + (uint64_t)x[i] * y[j] + (t >> 32);
			z[i + j] = (uint32_t)t;
		}
		if (t >>= 32) {
			z[i + j] = (uint32_t)t;
		}
	}
	*colz = colx + coly;
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
}

/* z = x^2 */
void bn_sqr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx)
{
	int32_t i, j = 0;
	uint64_t t;
	*colz = 2 * colx;
	memset(z, 0, sizeof(uint32_t) * *colz);
	for (i = 0; i < colx - 1; i++) {
		t = 0;
		for (j = i + 1; j < colx; j++) {
			t = z[i + j] + (uint64_t)x[i] * x[j] + (t >> 32);
			z[i + j] = (uint32_t)t;
		}
		if (t >>= 32) {
			z[i + j] = (uint32_t)t;
		}
	}
	for (i = i + j - 1; i >= 0; i--) {
		z[i + 1] |= z[i] >> 31;
		z[i] = z[i] << 1;
	}
	t = 0;
	for (i = 0; i < colx * 2; i++) {
		t = z[i] + (uint64_t)x[i >> 1] * x[i >> 1] + (t >> 32);
		z[i++] = (uint32_t)t;
		t = z[i] + (t >> 32);
		z[i] = (uint32_t)t;
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
}

/* z = x << n */
void bn_sftl(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n)
{
	int32_t i, q = n / 32, r = n & 0x1F;

	if (r == 0) {
		for (i = colx - 1; i >= 0; --i) {
			z[i + q] = x[i];
		}
		memset(z, 0, sizeof(int32_t) * q);
		*colz = colx + q;
	} else {
		int32_t col = 0;
		if (x[colx - 1] >> (32 - r)) {
			col++;
			z[colx + q] = 0;
		}
		for (i = colx - 1; i >= 0; --i) {
			z[i + q + 1] |= x[i] >> (32 - r);
			z[i + q] = x[i] << r;
		}
		memset(z, 0, sizeof(int32_t) * q);
		*colz = colx + q + col;
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
}

/* z = x >> n */
void bn_sftr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n)
{
	int32_t i, q = n / 32, r = n & 0x1F;

	if (r == 0) {
		if (colx - q <= 0) {
			*colz = 0;
		} else {
			for (i = q; i < colx; i++) {
				z[i - q] = x[i];
			}
			*colz = colx - q;
		}
	} else {
		if (colx - q <= 0) {
			*colz = 0;
		} else {
			int32_t col = 0;
			if ((x[colx - 1] >> r) == 0) {
				col--;
			}
			z[0] = x[q] >> r;
			for (i = q + 1; i < colx; i++) {
				z[i - q - 1] |= x[i] << (32 - r);
				z[i - q] = x[i] >> r;
			}
			*colz = colx - q + col;
		}
	}
	while (*colz > 0) {
		if (z[*colz - 1] > 0) {
			break;
		}
		(*colz)--;
	}
}

/* q = x / y + r */
void bn_div(uint32_t q[], int32_t *colq, uint32_t r[], int32_t *colr, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly)
{
	int32_t i;
	if (coly == 1) {
		uint64_t t = 0;
		for (i = colx - 1; i >= 0; i--) {
			t = t << 32 | x[i];
			q[i] = (uint32_t)(t / y[0]);
			t = t % y[0];
		}
		*colq = colx;
		while (*colq > 0) {
			if (q[*colq - 1] > 0) {
				break;
			}
			(*colq)--;
		}
		r[0] = (int32_t)t;
		*colr = 1;
	} else {
		uint32_t *t_a = r + *colr, *t_b = t_a + 1 + colx, *t_x = t_b + 1 + coly;
		int32_t cola, colb, d = 32 - find1_32(y[coly - 1]);
		if (d == 0) {
			memcpy(t_a, x, sizeof(uint32_t) * colx);
			memcpy(t_b, y, sizeof(uint32_t) * coly);
			cola = colx;
			colb = coly;
		} else {
			bn_sftl(t_a, &cola, x, colx, d);
			bn_sftl(t_b, &colb, y, coly, d);
		}
		i = cola - colb;
		if (i > 0) {
			*colq = i;
		} else if (i == 0) {
			if (bn_ge(t_a, cola, t_b, colb)) {
				q[0] = 1;
				*colq = 1;
				bn_sub(t_a, &cola, t_a, cola, t_b, colb);
			} else {
				*colq = 0;
			}
		} else {
			*colq = 0;
		}
		while (--i >= 0) {
			if (cola > 1 && bn_gt(t_a, cola, t_b, colb)) {
				uint64_t aa = (uint64_t)t_a[cola - 1] << 32 | t_a[cola - 2];
				uint64_t qq = aa / t_b[colb - 1];
				uint32_t q2[2];
				if (cola > 2 && colb > 1) {
					uint64_t rr = aa % t_b[colb - 1];
					while ((rr << 32 | t_a[cola - 3]) < qq * t_b[colb - 2]) {
						qq--;
						rr += t_b[colb - 2];
						if (rr > UINT32_MAX) break;
					}
				}
				do {
					q2[0] = (uint32_t)qq;
					q2[1] = (uint32_t)(qq >> 32);
					bn_mul(t_x, &colx, t_b, colb, q2, q2[1] > 0 ? 2 : 1);
					--qq;
					bn_sftl(t_x, &colx, t_x, colx, i * 32);
				} while (bn_gt(t_x, colx, t_a, cola));
				q[i] = (uint32_t)(qq + 1);
				if (qq >= UINT32_MAX) {
					q2[0] = 1;
					bn_add(&q[i + 1], colq, &q[i + 1], *colq - i - 1, q2, 1);
					*colq += i + 1;
				}
				bn_sub(t_a, &cola, t_a, cola, t_x, colx);
			} else if (bn_eq(t_a, cola, t_b, colb)) {
				q[i] = 1;
				while (i > 0) q[--i] = 0;
				while (cola > 0) t_a[--cola] = 0;
			} else {
				q[i] = 0;
			}
		}
		if (d > 0) {
			bn_sftr(r, colr, t_a, cola, d);
		} else {
			memcpy(r, t_a, cola * 32);
			*colr = cola;
			while (*colr > 0) {
				if (r[*colr - 1] > 0) {
					break;
				}
				(*colr)--;
			}
		}
	}
}

int32_t bn_str2num_base2(const char s[], int32_t len, uint32_t x[], int32_t col)
{
	int32_t i, j;
	memset(x, 0, col * sizeof(uint32_t));
	for (i = 0; s[i]; i++) {
		if (s[i] < '0' || '1' < s[i]) {
			return -1;
		}
		j = len - i - 1;
		x[j / 32] |= (s[i] - '0') << j % 32;
	}
	while (col > 0) {
		if (x[col - 1] > 0) {
			break;
		}
		col--;
	}
	return col;
}

int32_t bn_str2num_base8(const char s[], int32_t len, uint32_t x[], int32_t col)
{
	int32_t i, j, k;
	memset(x, 0, col * sizeof(uint32_t));
	for (i = 0; s[i]; i++) {
		if (s[i] < '0' || '7' < s[i]) {
			return -1;
		}
		k = (len - i - 1) / 32;
		j = (len - i - 1) % 32;
		if (j == 10) {
			x[k * 3 + 1] |= (uint32_t)(s[i] - '0') >> 2;
		} else if (j == 21) {
			x[k * 3 + 2] |= (uint32_t)(s[i] - '0') >> 1;
		}
		x[k * 3 + j / 11] |= (uint32_t)(s[i] - '0') << j % 11 * 3 << j / 11 % 3;
	}
	while (col > 0) {
		if (x[col - 1] > 0) {
			break;
		}
		col--;
	}
	return col;
}

int32_t bn_str2num_base10(const char s[], int32_t len, uint32_t x[], int32_t col)
{
	uint64_t t = 0;
	int32_t i, j, k = (len - 1) % 9 + 1;
	memset(x, 0, col * sizeof(uint32_t));
	for (col = 1, i = 0; s[i]; i++) {
		if (s[i] < '0' || '9' < s[i]) {
			return -1;
		}
		t = t * 10 + (uint64_t)(s[i] - '0');
		if (--k > 0) continue;
		t <<= 32;
		for (j = 0; j < col; j++) {
			t = (uint64_t)x[j] * 1000000000 + (t >> 32);
			x[j] = (uint32_t)t;
		}
		if (t >> 32) x[col++] = (uint32_t)(t >> 32);
		t = 0;
		k = 9;
	}
	while (col > 0) {
		if (x[col - 1] > 0) {
			break;
		}
		col--;
	}
	return col;
}

int32_t bn_str2num_base16(const char s[], int32_t len, uint32_t x[], int32_t col)
{
	int32_t i, j;
	memset(x, 0, col * sizeof(uint32_t));
	for (i = 0; s[i]; i++) {
		int c = toupper(s[i]);
		if ('0' <= c && c <= '9') {
			c -= '0';
		} else if ('A' <= c && c <= 'F') {
			c += 10 - 'A';
		} else {
			return -1;
		}
		j = len - i - 1;
		x[j / 8] |= c << j % 8 * 4;
	}
	while (col > 0) {
		if (x[col - 1] > 0) {
			break;
		}
		col--;
	}
	return col;
}

char *bn_num2str_base2(char *p, uint32_t x[], int32_t col)
{
	int32_t i, j;
	*p = 0;
	for (i = 0; i < col; i++) {
		for (j = 0; j < 32; j++) {
			uint32_t n = x[i] >> j;
			if (i < col - 1 || n != 0) {
				*--p = (n & 0x1) + '0';
			}
		}
	}
	return p;
}

char *bn_num2str_base8(char *p, uint32_t x[], int32_t col)
{
	int32_t i;
	*p = 0;
	for (i = 0; i < col; i++) {
		uint32_t j, k, m = x[i];
		if (i % 3 == 0) {
			k = 11;
		} else if (i % 3 == 1) {
			*p += (m & 0x1) << 2;
			k = 11;
		} else {
			*p += (m & 0x3) << 1;
			k = 10;
		}
		for (j = 0; j < k; j++) {
			uint32_t n = m >> (3 * j + i % 3);
			if (i < col - 1 || n != 0) {
				*--p = (n & 0x7) + '0';
			}
		}
	}
	return p;
}

char *bn_num2str_base10(char *p, uint32_t x[], int32_t col)
{
	*p = 0;
	while (col > 0) {
		int32_t i;
		uint64_t t = 0;
		for (i = col - 1; i >= 0; i--) {
			t = t << 32 | x[i];
			x[i] = (uint32_t)(t / 1000000000);
			t = t % 1000000000;
		}
		while (col > 0) {
			if (x[col - 1] > 0) {
				break;
			}
			col--;
		}
		for (i = 0; i < 9 && (col > 0 || t > 0); i++, t /= 10) {
			*--p = (char)(t % 10) + '0';
		}
	}
	return p;
}

char *bn_num2str_base16(char *p, uint32_t x[], int32_t col)
{
	int32_t i, j;
	*p = 0;
	for (i = 0; i < col; i++) {
		for (j = 0; j < 8; j++) {
			uint32_t n = x[i] >> (4 * j);
			if (i < col - 1 || n != 0) {
				char c = n & 0xf;
				*--p = (c < 10) ? c + '0' : c - 10 + 'a';
			}
		}
	}
	return p;
}
