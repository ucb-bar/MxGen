# Description

This repository provides configurable **low-precision floating-point hardware generators** for microscaled datapaths. Supported formats include **fp4 E2M1**, **fp6** with **E3M2** and **E2M3**, and **fp8** with **E5M2** and **E4M3**. The generator can be used to build a fused **processing element (PE)** that supports these formats.

The modes below correspond specifically to the **PE multiplication modes**.

## Mode 0

* 2 2b inputs (input A)
* 4 2b inputs (input B)
* 4 4b outputs

**Used by:**

* fp4 x fp4

---

## Mode 1

* 2 2b inputs (input A)
* 4 3b inputs (input B)
* 4 5b outputs

**Used by:**

* fp4 x fp6 (E3M2)
* fp4 x fp8 (E5M2)

---

## Mode 2

* 2 2b inputs (input A)
* 1 4b inputs (input B)
* 2 7b outputs

**Used by:**

* fp4 x fp6 (E2M3)
* fp4 x fp8 (E4M3)

---

## Mode 3

* 2 3b inputs (input A)
* 4 2b inputs (input B)
* 4 5b outputs

**Used by:**

* fp6 (E3M2) x fp4
* fp8 (E5M2) x fp4

---

## Mode 4

* 2 3b inputs (input A)
* 4 3b inputs (input B)
* 4 6b outputs

**Used by:**

* fp6 (E3M2) x fp6 (E3M2)
* fp6 (E3M2) x fp8 (E5M2)
* fp8 (E5M2) x fp6 (E3M2)
* fp8 (E5M2) x fp8 (E5M2)

---

## Mode 5

* 2 3b inputs (input A)
* 1 4b inputs (input B)
* 2 7b outputs

**Used by:**

* fp6 (E3M2) x fp6 (E2M3)
* fp6 (E3M2) x fp8 (E4M3)
* fp8 (E5M2) x fp6 (E2M3)
* fp8 (E5M2) x fp8 (E4M3)

---

## Mode 6

* 1 4b inputs (input A)
* 2 2b inputs (input B)
* 2 7b outputs

**Used by:**

* fp6 (E2M3) x fp4
* fp8 (E4M3) x fp4

---

## Mode 7

* 1 4b inputs (input A)
* 2 3b inputs (input B)
* 2 7b outputs

**Used by:**

* fp6 (E2M3) x fp6 (E3M2)
* fp6 (E2M3) x fp8 (E5M2)
* fp8 (E4M3) x fp6 (E3M2)
* fp8 (E4M3) x fp8 (E5M2)

---

## Mode 8

* 1 4b inputs (input A)
* 1 4b inputs (input B)
* 1 8b outputs

**Used by:**

* fp6 (E2M3) x fp6 (E2M3)
* fp6 (E2M3) x fp8 (E4M3)
* fp8 (E4M3) x fp6 (E2M3)
* fp8 (E4M3) x fp8 (E4M3)

