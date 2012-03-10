;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Error function

(define (error-function z)
  (error "Not implemented"))

;;; Complementary error function

(define (complementary-error-function z)
  (error "Not implemented"))

;;; TODO:

; Airy      GSL documentation Link
; 
; AiryAi(x) Airy Ai
; This routine compute the Airy function Ai(x)
; 
; AiryBi(x) Airy Bi
; This routine compute the Airy function Bi(x)
; 
; AiryAiScaled(x) Airy Ai scaled
; This routine compute a scaled version of the Airy function S_A(x) Ai(x). For x>0 the scaling factor S_A(x) is \exp(+(2/3) x^(3/2)), and is 1 for x<0
; 
; AiryBiScaled(x) Airy Bi scaled
; This routine compute a scaled version of the Airy function S_B(x) Bi(x). For x>0 the scaling factor S_B(x) is exp(-(2/3) x^(3/2)), and is 1 for x<0.
; 
; AiryAiDeriv(x)  Airy Ai derivate
; This routine compute the Airy function derivative Ai'(x)
; 
; AiryBiDeriv(x)  Airy Bi derivate
; This routine compute the Airy function derivative Bi'(x)
; 
; AiryAiDerivScaled(x)  Airy Ai derivate scaled
; This routine compute the scaled Airy function derivative S_A(x) Ai'(x). For x>0 the scaling factor S_A(x) is \exp(+(2/3) x^(3/2)), and is 1 for x<0
; 
; AiryBiDerivScaled(x)  Airy Bi derivate scaled
; This routine compute the scaled Airy function derivative S_B(x) Bi'(x). For x>0 the scaling factor S_B(x) is exp(-(2/3) x^(3/2)), and is 1 for x<0
; 
; AiryAiZero(s) Airy Ai zero
; This routine compute the location of the s-th zero of the Airy function Ai(x)
; 
; AiryBiZero(s) Airy Bi zero
; This routine compute the location of the s-th zero of the Airy function Bi(x)
; 
; AiryAiZeroDeriv(s)  Airy Ai zero derivate
; This routine compute the location of the s-th zero of the Airy function derivative Ai'(x)
; 
; AiryBiZeroDeriv(s)  Airy Bi zero derivate
; This routine compute the location of the s-th zero of the Airy function derivative Bi'(x).
; 
; 
; 
; 
; Bessel      GSL documentation Link
; 
; BesselJ0(x) Bessel regular cylindrical J0
; These routines compute the regular cylindrical Bessel function of zeroth order, J_0(x)
; 
; BesselJ1(x) Bessel regular cylindrical J1
; These routines compute the regular cylindrical Bessel function of first order, J_1(x).
; 
; BesselJn(x,n) Bessel regular cylindrical Jn
; These routines compute the regular cylindrical Bessel function of order n, J_n(x).
; 
; BesselY0(x) Bessel irregular cylindrical Y0
; These routines compute the irregular cylindrical Bessel function of zeroth order, Y_0(x), for x>0.
; 
; BesselY1(x) Bessel irregular cylindrical Y1
; These routines compute the irregular cylindrical Bessel function of first order, Y_1(x), for x>0.
; 
; BesselYn(x,n) Bessel irregular cylindrical Yn
; These routines compute the irregular cylindrical Bessel function of order n, Y_n(x), for x>0.
; 
; BesselI0(x) Bessel regular modified cylindrical I0
; These routines compute the regular modified cylindrical Bessel function of zeroth order, I_0(x).
; 
; BesselI1(x) Bessel regular modified cylindrical I1
; These routines compute the regular modified cylindrical Bessel function of first order, I_1(x).
; 
; BesselIn(x,n) Bessel regular modified cylindrical In
; These routines compute the regular modified cylindrical Bessel function of order n, I_n(x).
; 
; BesselI0Scaled(x) Bessel regular modified cylindrical I0 scaled
; These routines compute the scaled regular modified cylindrical Bessel function of zeroth order \exp(-|x|) I_0(x).
; 
; BesselI1Scaled(x) Bessel regular modified cylindrical I1 scaled
; These routines compute the scaled regular modified cylindrical Bessel function of first order \exp(-|x|) I_1(x).
; 
; BesselInScaled(x,n) Bessel regular modified cylindrical In scaled
; These routines compute the scaled regular modified cylindrical Bessel function of order n, \exp(-|x|) I_n(x)
; 
; BesselK0(x) Bessel irregular modified cylindrical K0
; These routines compute the irregular modified cylindrical Bessel function of zeroth order, K_0(x), for x > 0.
; 
; BesselK1(x) Bessel irregular modified cylindrical K1
; These routines compute the irregular modified cylindrical Bessel function of first order, K_1(x), for x > 0.
; 
; BesselKn(x,n) Bessel irregular modified cylindrical Kn
; These routines compute the irregular modified cylindrical Bessel function of order n, K_n(x), for x > 0.
; 
; BesselK0Scaled(x) Bessel irregular modified cylindrical K0 scaled
; These routines compute the scaled irregular modified cylindrical Bessel function of zeroth order \exp(x) K_0(x) for x>0.
; 
; BesselK1Scaled(x) Bessel irregular modified cylindrical K1 scaled
; These routines compute the scaled irregular modified cylindrical Bessel function of first order \exp(x) K_1(x) for x>0.
; 
; BesselKnScaled(x,n) Bessel irregular modified cylindrical Kn scaled
; These routines compute the scaled irregular modified cylindrical Bessel function of order n, \exp(x) K_n(x), for x>0.
; 
; Besselj0(x) Bessel regular spherical j0
; These routines compute the regular spherical Bessel function of zeroth order, j_0(x) = \sin(x)/x.
; 
; Besselj1(x) Bessel regular spherical j1
; These routines compute the regular spherical Bessel function of first order, j_1(x) = (\sin(x)/x - \cos(x))/x.
; 
; Besselj2(x) Bessel regular spherical j2
; These routines compute the regular spherical Bessel function of second order, j_2(x) = ((3/x^2 - 1)\sin(x) - 3\cos(x)/x)/x.
; 
; Besseljn(x,n) Bessel regular spherical jn
; These routines compute the regular spherical Bessel function of order n, j_n(x), for n >= 0 and x >= 0.
; 
; Bessely0(x) Bessel irregular spherical y0
; These routines compute the irregular spherical Bessel function of zeroth order, y_0(x) = -\cos(x)/x.
; 
; Bessely1(x) Bessel irregular spherical y1
; These routines compute the irregular spherical Bessel function of first order, y_1(x) = -(\cos(x)/x + \sin(x))/x.
; 
; Bessely2(x) Bessel irregular spherical y2
; These routines compute the irregular spherical Bessel function of second order, y_2(x) = (-3/x^3 + 1/x)\cos(x) - (3/x^2)\sin(x).
; 
; Besselyn(x,n) Bessel irregular spherical yn
; These routines compute the irregular spherical Bessel function of order n, y_n(x), for n >= 0.
; 
; Besseli0Scaled(x) Bessel regular modified spherical i0 scaled
; These routines compute the scaled regular modified spherical Bessel function of zeroth order, \exp(-|x|) i_0(x).
; 
; Besseli1Scaled(x) Bessel regular modified spherical i1 scaled
; These routines compute the scaled regular modified spherical Bessel function of first order, \exp(-|x|) i_1(x).
; 
; Besseli2Scaled(x) Bessel regular modified spherical i2 scaled
; These routines compute the scaled regular modified spherical Bessel function of second order, \exp(-|x|) i_2(x)
; 
; BesselinScaled(x,n) Bessel regular modified spherical in scaled
; These routines compute the scaled regular modified spherical Bessel function of order n, \exp(-|x|) i_n(x)
; 
; Besselk0Scaled(x) Bessel irregular modified spherical k0 scaled
; These routines compute the scaled irregular modified spherical Bessel function of zeroth order, \exp(x) k_0(x), for x>0.
; 
; Besselk1Scaled(x) Bessel irregular modified spherical k1 scaled
; These routines compute the scaled irregular modified spherical Bessel function of first order, \exp(x) k_1(x), for x>0.
; 
; Besselk2Scaled(x) Bessel irregular modified spherical k2 scaled
; These routines compute the scaled irregular modified spherical Bessel function of second order, \exp(x) k_2(x), for x>0.
; 
; BesselknScaled(x,n) Bessel irregular modified spherical kn scaled
; These routines compute the scaled irregular modified spherical Bessel function of order n, \exp(x) k_n(x), for x>0.
; 
; BesselJnu(x,nu) Bessel regular cylindrical of fractional order Jnu
; These routines compute the regular cylindrical Bessel function of fractional order \nu, J_\nu(x).
; 
; BesselYnu(x,nu) Bessel irregular cylindrical of fractional order Ynu
; These routines compute the irregular cylindrical Bessel function of fractional order \nu, Y_\nu(x).
; 
; BesselInu(x,nu) Bessel regular modified cylindrical of fractional order Inu
; These routines compute the regular modified Bessel function of fractional order \nu, I_\nu(x) for x>0, \nu>0.
; 
; BesselKnu(x,nu) Bessel irregular modified cylindrical of fractional order Knu
; These routines compute the irregular modified Bessel function of fractional order \nu, K_\nu(x) for x>0, \nu>0.
; 
; BessellnKnu(x,nu) Bessel ln of irregular modified cylindrical of fractional order Knu
; These routines compute the logarithm of the irregular modified Bessel function of fractional order \nu, \ln(K_\nu(x)) for x>0, \nu>0.
; 
; BesselKnuScaled(x,nu) Bessel irregular modified cylindrical of fractional order Knu scaled
; These routines compute the scaled irregular modified Bessel function of fractional order \nu, \exp(+|x|) K_\nu(x) for x>0, \nu>0.
; 
; BesselJ0Zero(s) Bessel regular cylindrical J0 zero
; These routines compute the location of the s-th positive zero of the Bessel function J_0(x).
; 
; BesselJ1Zero(s) Bessel regular cylindrical J1 zero
; These routines compute the location of the s-th positive zero of the Bessel function J_1(x).
; 
; BesselJnuZero(s,nu) Bessel regular cylindrical of fractional order Jnu zero
; These routines compute the location of the s-th positive zero of the Bessel function J_\nu(x). The current implementation does not support negative values of nu.
; 
; 
; 
; 
; Clausen      GSL documentation Link
; 
; Clausen(x)  Clausen
; These routines compute the Clausen integral Cl_2(x).
; 
; 
; 
; 
; Dawson      GSL documentation Link
; 
; Dawson(x) Dawson
; These routines compute the value of Dawson's integral for x.
; 
; 
; 
; 
; Debye      GSL documentation Link
; 
; Debye1(x) Debye order 1
; These routines compute the first-order Debye function D_1(x) = (1/x) \int_0^x dt (t/(e^t - 1)).
; 
; Debye2(x) Debye order 2
; These routines compute the second-order Debye function D_2(x) = (2/x^2) \int_0^x dt (t^2/(e^t - 1)).
; 
; Debye3(x) Debye order 3
; These routines compute the third-order Debye function D_3(x) = (3/x^3) \int_0^x dt (t^3/(e^t - 1)).
; 
; Debye4(x) Debye order 4
; These routines compute the fourth-order Debye function D_4(x) = (4/x^4) \int_0^x dt (t^4/(e^t - 1)).
; 
; Debye5(x) Debye order 5
; These routines compute the fifth-order Debye function D_5(x) = (5/x^5) \int_0^x dt (t^5/(e^t - 1)).
; 
; Debye6(x) Debye order 6
; These routines compute the sixth-order Debye function D_6(x) = (6/x^6) \int_0^x dt (t^6/(e^t - 1)).
; 
; 
; 
; 
; Dilogarithm      GSL documentation Link
; 
; Dilogarithm(x)  Dilogarithm
; These routines compute the dilogarithm for a real argument. In Lewin's notation this is Li_2(x), the real part of the dilogarithm of a real x. It is defined by the integral representation Li_2(x) = - \Re \int_0^x ds \log(1-s) / s. Note that \Im(Li_2(x)) = 0 for x <= 1, and -\pi\log(x) for x > 1.
; 
; 
; 
; 
; Elliptic integrals      GSL documentation Link
; 
; EllintKcomp(k)  Elliptic integrals complete Legendre form K
; These routines compute the complete elliptic integral K(k)
; 
; EllintEcomp(k)  Elliptic integrals complete Legendre form E
; These routines compute the complete elliptic integral E(k)
; 
; EllintPcomp(k,n)  Elliptic integrals complete Legendre form P
; These routines compute the complete elliptic integral \Pi(k,n)
; 
; EllintF(phi,k)  Elliptic integrals incomplete Legendre form F
; These routines compute the incomplete elliptic integral F(\phi,k)
; 
; EllintE(phi,k)  Elliptic integrals incomplete Legendre form E
; These routines compute the incomplete elliptic integral E(\phi,k)
; 
; EllintP(phi,k,n)  Elliptic integrals incomplete Legendre form P
; These routines compute the incomplete elliptic integral \Pi(\phi,k,n)
; 
; EllintD(phi,k)  Elliptic integrals incomplete Legendre form D
; These functions compute the incomplete elliptic integral D(\phi,k)
; 
; EllintRC(x,y) Elliptic integrals incomplete Carlson form RC
; These routines compute the incomplete elliptic integral RC(x,y)
; 
; EllintRD(x,y,z) Elliptic integrals incomplete Carlson form RD
; These routines compute the incomplete elliptic integral RD(x,y,z)
; 
; EllintRF(x,y,z) Elliptic integrals incomplete Carlson form RF
; These routines compute the incomplete elliptic integral RF(x,y,z)
; 
; EllintRJ(x,y,z,p) Elliptic integrals incomplete Carlson form RJ
; These routines compute the incomplete elliptic integral RJ(x,y,z,p)
; 
; EllfuncSn(u,m)  Elliptic functions of Jacobi sn
; This function computes the Jacobian elliptic functions sn(u,m)
; 
; EllfuncCn(u,m)  Elliptic functions of Jacobi cn
; This function computes the Jacobian elliptic functions cn(u,m)
; 
; EllfuncDn(u,m)  Elliptic functions of Jacobi dn
; This function computes the Jacobian elliptic functions dn(u,m)
; 
; 
; 
; 
; Error function      GSL documentation Link
; 
; Erf(x)  Error function erf
; These routines compute the error function erf(x), where erf(x) = (2/\sqrt(\pi)) \int_0^x dt \exp(-t^2).
; 
; Erfc(x) Error function complementary erfc
; These routines compute the complementary error function erfc(x) = 1 - erf(x) = (2/\sqrt(\pi)) \int_x^\infty \exp(-t^2).
; 
; LnErfc(x) Error function log complementary log(erfc)
; These routines compute the logarithm of the complementary error function \log(\erfc(x)).
; 
; ErfZ(x) Probability, gaussian density function
; These routines compute the Gaussian probability density function Z(x) = (1/\sqrt{2\pi}) \exp(-x^2/2).
; 
; ErfQ(x) Probability, upper tail gaussian density function
; These routines compute the upper tail of the Gaussian probability function Q(x) = (1/\sqrt{2\pi}) \int_x^\infty dt \exp(-t^2/2).
; 
; Hazard(x) Probability, hazard function for the normal distribution
; These routines compute the hazard function for the normal distribution.The hazard function for the normal distribution, also known as the inverse Mill's ratio, is defined as, h(x) = Z(x)/Q(x) = \sqrt{2/\pi} \exp(-x^2 / 2) / \erfc(x/\sqrt 2)
; 
; 
; 
; 
; Exponential integral      GSL documentation Link
; 
; ExpIntE1(x) Exponential integral E1
; These routines compute the exponential integral E_1(x), E_1(x) := \Re \int_1^\infty dt \exp(-xt)/t.
; 
; ExpIntE2(x) Exponential integral E2
; These routines compute the second-order exponential integral E_2(x), E_2(x) := \Re \int_1^\infty dt \exp(-xt)/t^2.
; 
; ExpIntEn(n,x) Exponential integral En
; These routines compute the exponential integral E_n(x) of order n, E_n(x) := \Re \int_1^\infty dt \exp(-xt)/t^n.
; 
; ExpIntEi(x) Exponential integral Ei
; These routines compute the exponential integral Ei(x), Ei(x) := - PV(\int_{-x}^\infty dt \exp(-t)/t) where PV denotes the principal value of the integral.
; 
; ExpIntEi3(x)  Exponential integral third order Ei_3
; These routines compute the third-order exponential integral Ei_3(x) = \int_0^xdt \exp(-t^3) for x >= 0.
; 
; 
; 
; 
; Hyperbolic integral      GSL documentation Link
; 
; Shi(x)  Hyperbolic integral Shi
; These routines compute the integral Shi(x) = \int_0^x dt \sinh(t)/t.
; 
; Chi(x)  Hyperbolic integral Chi
; These routines compute the integral Chi(x) := \Re[ \gamma_E + \log(x) + \int_0^x dt (\cosh[t]-1)/t] , where \gamma_E is the Euler constant
; 
; 
; 
; 
; Sine/Cosine integral      GSL documentation Link
; 
; Si(x) Sine integral Si
; These routines compute the Sine integral Si(x) = \int_0^x dt \sin(t)/t.
; 
; Ci(x) Cosine integral Ci
; These routines compute the Cosine integral Ci(x) = -\int_x^\infty dt \cos(t)/t for x > 0.
; 
; 
; 
; 
; Arctangent integral      GSL documentation Link
; 
; AtanInt(x)  Arctangent integral AtanInt
; These routines compute the Arctangent integral, which is defined as AtanInt(x) = \int_0^x dt \arctan(t)/t.
; 
; 
; 
; 
; Fermi-Dirac      GSL documentation Link
; 
; FermiDiracM1(x) Complete Fermi-Dirac integral of index -1
; These routines compute the complete Fermi-Dirac integral with an index of -1. This integral is given by F_{-1}(x) = e^x / (1 + e^x). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDirac0(x)  Complete Fermi-Dirac integral of index 0
; These routines compute the complete Fermi-Dirac integral with an index of 0. This integral is given by F_0(x) = \ln(1 + e^x). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDirac1(x)  Complete Fermi-Dirac integral of index 1
; These routines compute the complete Fermi-Dirac integral with an index of 1, F_1(x) = \int_0^\infty dt (t /(\exp(t-x)+1)). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDirac2(x)  Complete Fermi-Dirac integral of index 2
; These routines compute the complete Fermi-Dirac integral with an index of 2, F_2(x) = (1/2) \int_0^\infty dt (t^2 /(\exp(t-x)+1)). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDiracj(j,x)  Complete Fermi-Dirac integral of index j
; These routines compute the complete Fermi-Dirac integral with an integer index of j, F_j(x) = (1/\Gamma(j+1)) \int_0^\infty dt (t^j /(\exp(t-x)+1)). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDiracMhalf(x)  Complete Fermi-Dirac integral of index -1/2
; These routines compute the complete Fermi-Dirac integral F_{-1/2}(x). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDirachalf(x) Complete Fermi-Dirac integral of index 1/2
; These routines compute the complete Fermi-Dirac integral F_{1/2}(x). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDirac3half(x)  Complete Fermi-Dirac integral of index 3/2
; These routines compute the complete Fermi-Dirac integral F_{3/2}(x). The complete Fermi-Dirac integral F_j(x) is given by,F_j(x) := (1/\Gamma(j+1)) \int_0^\infty dt (t^j / (\exp(t-x) + 1))
; 
; FermiDiracIncj(x,b) Incomplete Fermi-Dirac integral of index j
; These routines compute the incomplete Fermi-Dirac integral with an index of zero, F_0(x,b) = \ln(1 + e^{b-x}) - (b-x). The incomplete Fermi-Dirac integral F_j(x,b) is given by, F_j(x,b) := (1/\Gamma(j+1)) \int_b^\infty dt (t^j / (\Exp(t-x) + 1))
; 
; 
; 
; 
; Gamma function      GSL documentation Link
; 
; Gamma(x)  Gamma
; These routines compute the Gamma function \Gamma(x), subject to x not being a negative integer or zero.The Gamma function is defined by the following integral, \Gamma(x) = \int_0^\infty dt t^{x-1} \exp(-t)
; 
; LnGamma(x)  Ln Gamma
; These routines compute the logarithm of the Gamma function, \log(\Gamma(x)), subject to x not being a negative integer or zero.
; 
; GammaStar(x)  Gamma star
; These routines compute the regulated Gamma Function \Gamma^*(x) for x > 0. The regulated gamma function is given by, \Gamma^*(x) = \Gamma(x)/(\sqrt{2\pi} x^{(x-1/2)} \exp(-x)) = (1 + (1/12x) + ...) for x \to \infty
; 
; GammaInv(x) Gamma inv
; These routines compute the reciprocal of the gamma function, 1/\Gamma(x)
; 
; GammaInc(a,x) Unnormalized incomplete Gamma Function
; These functions compute the unnormalized incomplete Gamma Function \Gamma(a,x) = \int_x^\infty dt t^{a-1} \exp(-t) for a real and x >= 0.
; 
; GammaIncQ(a,x)  Normalized incomplete Gamma Function
; These routines compute the normalized incomplete Gamma Function Q(a,x) = 1/\Gamma(a) \int_x^\infty dt t^{a-1} \exp(-t) for a > 0, x >= 0.
; 
; GammaIncP(a,x)  Complementary Normalized incomplete Gamma Function
; These routines compute the complementary normalized incomplete Gamma Function P(a,x) = 1 - Q(a,x) = 1/\Gamma(a) \int_0^x dt t^{a-1} \exp(-t) for a > 0, x >= 0.
; 
; 
; 
; 
; Factorial/Combination/Pochhamer      GSL documentation Link
; 
; Fact(n) Factorial
; These routines compute the factorial n!. The factorial is related to the Gamma function by n! = \Gamma(n+1).
; 
; DoubleFact(n) Double Factorial
; These routines compute the double factorial n!! = n(n-2)(n-4) \dots
; 
; LnFact(n) Ln Factorial
; These routines compute the logarithm of the factorial of n, \log(n!).
; 
; LnDoubleFact(n) Ln Double Factorial
; These routines compute the logarithm of the double factorial of n, \log(n!!).
; 
; Choose(n,m) Combination
; These routines compute the combinatorial factor n choose m = n!/(m!(n-m)!)
; 
; LnChoose(n,m) Ln Combination
; These routines compute the logarithm of n choose m. This is equivalent to the sum \log(n!) - \log(m!) - \log((n-m)!).
; 
; TaylorCoef(n,x) Taylor coefficient
; These routines compute the Taylor coefficient x^n / n! for x >= 0, n >= 0.
; 
; Poch(a,x) Pochhammer
; These routines compute the Pochhammer symbol (a)_x = \Gamma(a + x)/\Gamma(a), subject to a and a+x not being negative integers or zero.
; 
; LnPoch(a,x) Ln Pochhammer
; These routines compute the logarithm of the Pochhammer symbol, \log((a)_x) = \log(\Gamma(a + x)/\Gamma(a)) for a > 0, a+x > 0.
; 
; PochRel(a,x)  Pochhammer relative
; These routines compute the relative Pochhammer symbol ((a)_x - 1)/x where (a)_x = \Gamma(a + x)/\Gamma(a).
; 
; 
; 
; 
; Beta      GSL documentation Link
; 
; Beta(a,b) Beta
; These routines compute the Beta Function, B(a,b) = \Gamma(a)\Gamma(b)/\Gamma(a+b) subject to a and b not being negative integers.
; 
; LnBeta(a,b) Ln Beta
; These routines compute the logarithm of the Beta Function, \log(B(a,b)) subject to a and b not being negative integers.
; 
; BetaInc(a,b,x)  Incomplete Beta
; These routines compute the normalized incomplete Beta function I_x(a,b)=B_x(a,b)/B(a,b) where B_x(a,b) = \int_0^x t^{a-1} (1-t)^{b-1} dt for 0 <= x <= 1.
; 
; 
; 
; 
; Gegenbauer      GSL documentation Link
; 
; GegenPoly1(lambda,x)  Gegenbauer Functions 1
; These functions evaluate the Gegenbauer polynomials C^{(\lambda)}_n(x) for n =1
; 
; GegenPoly2(lambda,x)  Gegenbauer Functions 2
; These functions evaluate the Gegenbauer polynomials C^{(\lambda)}_n(x) for n =2
; 
; GegenPoly3(lambda,x)  Gegenbauer Functions 3
; These functions evaluate the Gegenbauer polynomials C^{(\lambda)}_n(x) for n =3
; 
; GegenPolyn(n,lambda,x)  Gegenbauer Functions n
; These functions evaluate the Gegenbauer polynomial C^{(\lambda)}_n(x) for a specific value of n, lambda, x subject to \lambda > -1/2, n >= 0.
; 
; 
; 
; 
; Hypergeometric      GSL documentation Link
; 
; Hyperg0F1(c,x)  Hypergeometric 0F1
; These routines compute the hypergeometric function 0F1(c,x).
; 
; Hyperg1F1int(m,n,x) Hypergeometric 1F1 int
; These routines compute the confluent hypergeometric function 1F1(m,n,x) = M(m,n,x) for integer parameters m, n.
; 
; Hyperg1F1(a,b,x)  Hypergeometric 1F1
; These routines compute the confluent hypergeometric function 1F1(a,b,x) = M(a,b,x) for general parameters a, b.
; 
; HypergUint(m,n,x) Hypergeometric U int
; These routines compute the confluent hypergeometric function U(m,n,x) for integer parameters m, n.
; 
; HypergU(a,b,x)  Hypergeometric U
; These routines compute the confluent hypergeometric function U(a,b,x).
; 
; Hyperg2F1(a,b,c,x)  Gauss Hypergeometric 2F1
; These routines compute the Gauss hypergeometric function 2F1(a,b,c,x) = F(a,b,c,x) for |x| < 1.
; 
; Hyperg2F1conj(a_R,a_I,c,x)  Gauss Hypergeometric 2F1 conj
; These routines compute the Gauss hypergeometric function 2F1(a_R + i a_I, a_R - i a_I, c, x) with complex parameters for |x| < 1.
; 
; Hyperg2F1renor(a,b,c,x) Gauss Hypergeometric 2F1 renorm
; These routines compute the renormalized Gauss hypergeometric function 2F1(a,b,c,x) / \Gamma(c) for |x| < 1.
; 
; Hyperg2F1conjrenor(a_R,a_I,c,x) Gauss Hypergeometric 2F1 conj renorm
; These routines compute the renormalized Gauss hypergeometric function 2F1(a_R + i a_I, a_R - i a_I, c, x) / \Gamma(c) for |x| < 1.
; 
; Hyperg2F0(a,b,x)  Hypergeometric 2F0
; These routines compute the hypergeometric function 2F0(a,b,x). The series representation is a divergent hypergeometric series. However, for x < 0 we have 2F0(a,b,x) = (-1/x)^a U(a,1+a-b,-1/x)
; 
; 
; 
; 
; Laguerre      GSL documentation Link
; 
; Laguerre1(a,x)  Laguerre Functions 1
; These routines evaluate the generalized Laguerre polynomials L^a_1(x)
; 
; Laguerre2(a,x)  Laguerre Functions 2
; These routines evaluate the generalized Laguerre polynomials L^a_2(x)
; 
; Laguerre3(a,x)  Laguerre Functions 3
; These routines evaluate the generalized Laguerre polynomials L^a_3(x)
; 
; Laguerren(n,a,x)  Laguerre Functions n
; These routines evaluate the generalized Laguerre polynomials L^a_n(x) for a > -1, n >= 0.
; 
; 
; 
; 
; Lambert      GSL documentation Link
; 
; LambertW0(x)  Lambert W0
; These compute the principal branch of the Lambert W function, W_0(x).
; 
; LambertWm1(x) Lambert Wm1
; These compute the secondary real-valued branch of the Lambert W function, W_{-1}(x).
; 
; 
; 
; 
; Legendre      GSL documentation Link
; 
; LegendreP1(x) Legendre P1
; These functions evaluate the Legendre polynomials P_n(x) using explicit representations for n=1
; 
; LegendreP2(x) Legendre P2
; These functions evaluate the Legendre polynomials P_n(x) using explicit representations for n=2
; 
; LegendreP3(x) Legendre P3
; These functions evaluate the Legendre polynomials P_n(x) using explicit representations for n=3
; 
; LegendrePn(n,x) Legendre Pn
; These functions evaluate the Legendre polynomial P_n(x) for a specific value of n, x subject to n >= 0, |x| <= 1
; 
; LegendreQ0(x) Legendre Q0
; These routines compute the Legendre function Q_0(x) for x > -1, x != 1.
; 
; LegendreQ1(x) Legendre Q1
; These routines compute the Legendre function Q_1(x) for x > -1, x != 1.
; 
; LegendreQn(n,x) Legendre Qn
; These routines compute the Legendre function Q_n(x) for x > -1, x != 1 and n >= 0.
; 
; LegendrePlm(l,m,x)  Associated Legendre
; These routines compute the associated Legendre polynomial P_l^m(x) for m >= 0, l >= m, |x| <= 1.
; 
; LegendreSphPlm(l,m,x) Normalized Associated Legendre
; These routines compute the normalized associated Legendre polynomial $\sqrt{(2l+1)/(4\pi)} \sqrt{(l-m)!/(l+m)!} P_l^m(x)$ suitable for use in spherical harmonics. The parameters must satisfy m >= 0, l >= m, |x| <= 1
; 
; 
; 
; 
; Digamma/Trigamma/Polygamma      GSL documentation Link
; 
; DigammaInt(n) Digamma for integer
; These routines compute the digamma function \psi(n) for positive integer n. The digamma function is also called the Psi function.
; 
; Digamma(x)  Digamma
; These routines compute the digamma function \psi(x) for general x, x \ne 0.
; 
; DigammaPiy(y) Real part of the digamma function on the line 1+iy
; These routines compute the real part of the digamma function on the line 1+i y, \Re[\psi(1 + i y)].
; 
; TrigammaInt(n)  Trigamma for integer
; These routines compute the Trigamma function \psi'(n) for positive integer n.
; 
; Trigamma(x) Trigamma
; These routines compute the Trigamma function \psi'(x) for general x.
; 
; Polygamma(n,x)  Polygamma
; These routines compute the polygamma function \psi^{(n)}(x) for n >= 0, x > 0.
; 
; 
; 
; 
; Synchrotron      GSL documentation Link
; 
; Synchrotron1(x) Synchrotron Function 1
; These routines compute the first synchrotron function x \int_x^\infty dt K_{5/3}(t) for x >= 0.
; 
; Synchrotron2(x) Synchrotron Function 2
; These routines compute the second synchrotron function x K_{2/3}(x) for x >= 0.
; 
; 
; 
; 
; Transport      GSL documentation Link
; 
; Transport2(x) Transport Function 2
; These routines compute the transport function J(2,x).The transport functions J(n,x) are defined by the integral representations J(n,x) := \int_0^x dt t^n e^t /(e^t - 1)^2.
; 
; Transport3(x) Transport Function 3
; These routines compute the transport function J(3,x).The transport functions J(n,x) are defined by the integral representations J(n,x) := \int_0^x dt t^n e^t /(e^t - 1)^2.
; 
; Transport4(x) Transport Function 4
; These routines compute the transport function J(4,x).The transport functions J(n,x) are defined by the integral representations J(n,x) := \int_0^x dt t^n e^t /(e^t - 1)^2.
; 
; Transport5(x) Transport Function 5
; These routines compute the transport function J(5,x).The transport functions J(n,x) are defined by the integral representations J(n,x) := \int_0^x dt t^n e^t /(e^t - 1)^2.
; 
; 
; 
; 
; Zeta/Eta      GSL documentation Link
; 
; ZetaInt(n)  Rieman Zeta Function int
; These routines compute the Riemann zeta function \zeta(n) for integer n, n \ne 1.
; 
; Zeta(s) Rieman Zeta Function
; These routines compute the Riemann zeta function \zeta(s) for arbitrary s, s \ne 1.
; 
; Zetam1Int(n)  Rieman Zeta Function minus one int
; These routines compute \zeta(n) - 1 for integer n, n \ne 1.
; 
; Zetam1(s) Rieman Zeta Function minus one
; These routines compute \zeta(s) - 1 for arbitrary s, s \ne 1.
; 
; HurwitzZeta(s,q)  Hurwitz Zeta Function
; These routines compute the Hurwitz zeta function \zeta(s,q) for s > 1, q > 0.
; 
; EtaInt(n) Eta Function int
; These routines compute the eta function \eta(n) for integer n.
; 
; Eta(s)  Eta Function
; These routines compute the eta function \eta(s) for arbitrary s.
