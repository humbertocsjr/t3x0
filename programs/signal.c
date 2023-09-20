/*
 * signal() function for T3X/0.
 * Nils M Holm, 20220
 * Public domain / 0BSD license
 *
 * Declare with EXTERN signal(2);
 * Usage: signal(sig_no, @procedure);
 */

#include <signal.h>

int t3x_signal(void (*fn)(), int sig) {
	signal(sig, fn);
	return 0;
}
