/*******************************************************************************
 * Copyright (c) 2000, 2019 IBM Corp. and others
 *
 * This program and the accompanying materials are made available under
 * the terms of the Eclipse Public License 2.0 which accompanies this
 * distribution and is available at http://eclipse.org/legal/epl-2.0
 * or the Apache License, Version 2.0 which accompanies this distribution
 * and is available at https://www.apache.org/licenses/LICENSE-2.0.
 *
 * This Source Code may also be made available under the following Secondary
 * Licenses when the conditions for such availability set forth in the
 * Eclipse Public License, v. 2.0 are satisfied: GNU General Public License,
 * version 2 with the GNU Classpath Exception [1] and GNU General Public
 * License, version 2 with the OpenJDK Assembly Exception [2].
 *
 * [1] https://www.gnu.org/software/classpath/license.html
 * [2] http://openjdk.java.net/legal/assembly-exception.html
 *
 * SPDX-License-Identifier: EPL-2.0 OR Apache-2.0 OR GPL-2.0 WITH Classpath-exception-2.0 OR LicenseRef-GPL-2.0 WITH Assembly-exception
 *******************************************************************************/

#include <stdint.h>
#include <stdio.h>
#include "compile/Method.hpp"
#include "env/jittypes.h"
#include "gtest/gtest.h"

#include "tests/S390OpCodesTest.hpp"

namespace TestCompiler
{
void
S390OpCodesTest::compileIntegerArithmeticTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_lAdd, _numberOfBinaryArgs, TR::ladd, "lAdd", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_lSub, _numberOfBinaryArgs, TR::lsub, "lSub", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_lMul, _numberOfBinaryArgs, TR::lmul, "lMul", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_lDiv, _numberOfBinaryArgs, TR::ldiv, "lDiv", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_lRem, _numberOfBinaryArgs, TR::lrem, "lRem", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_luDiv, _numberOfBinaryArgs, TR::ludiv, "luDiv", _argTypesBinaryLong, TR::Int64, rc);

   compileOpCodeMethod(_iuMulh, _numberOfBinaryArgs, TR::iumulh, "iuMulh", _argTypesBinaryInt, TR::Int32, rc);
   }

void
S390OpCodesTest::compileFloatArithmeticTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_fAdd, _numberOfBinaryArgs, TR::fadd, "fAdd", _argTypesBinaryFloat, TR::Float, rc);
   compileOpCodeMethod(_fSub, _numberOfBinaryArgs, TR::fsub, "fSub", _argTypesBinaryFloat, TR::Float, rc);
   compileOpCodeMethod(_fMul, _numberOfBinaryArgs, TR::fmul, "fMul", _argTypesBinaryFloat, TR::Float, rc);
   compileOpCodeMethod(_fDiv, _numberOfBinaryArgs, TR::fdiv, "fDiv", _argTypesBinaryFloat, TR::Float, rc);
   compileOpCodeMethod(_dAdd, _numberOfBinaryArgs, TR::dadd, "dAdd", _argTypesBinaryDouble, TR::Double, rc);
   compileOpCodeMethod(_dSub, _numberOfBinaryArgs, TR::dsub, "dSub", _argTypesBinaryDouble, TR::Double, rc);
   compileOpCodeMethod(_dMul, _numberOfBinaryArgs, TR::dmul, "dMul", _argTypesBinaryDouble, TR::Double, rc);
   compileOpCodeMethod(_dDiv, _numberOfBinaryArgs, TR::ddiv, "dDiv", _argTypesBinaryDouble, TR::Double, rc);

   }

void
S390OpCodesTest::compileMemoryOperationTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_lStore, _numberOfUnaryArgs, TR::lstore, "lStore", _argTypesUnaryLong, TR::Int64, rc);
   compileOpCodeMethod(_dStore, _numberOfUnaryArgs, TR::dstore, "dStore", _argTypesUnaryDouble, TR::Double, rc);
   compileOpCodeMethod(_fStore, _numberOfUnaryArgs, TR::fstore, "fStore", _argTypesUnaryFloat, TR::Float, rc);

   compileOpCodeMethod(_iStorei, _numberOfBinaryArgs, TR::istorei, "iStorei", _argTypesBinaryAddressInt, TR::Int32, rc);
   compileOpCodeMethod(_lStorei, _numberOfBinaryArgs, TR::lstorei, "lStorei", _argTypesBinaryAddressLong, TR::Int64, rc);
   compileOpCodeMethod(_dStorei, _numberOfBinaryArgs, TR::dstorei, "dStorei", _argTypesBinaryAddressDouble, TR::Double, rc);
   compileOpCodeMethod(_fStorei, _numberOfBinaryArgs, TR::fstorei, "fStorei", _argTypesBinaryAddressFloat, TR::Float, rc);
   compileOpCodeMethod(_aStorei, _numberOfBinaryArgs, TR::astorei, "aStorei", _argTypesBinaryAddressAddress, TR::Address, rc);
   }

void
S390OpCodesTest::compileUnaryTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_lAbs, _numberOfUnaryArgs, TR::labs, "lAbs", _argTypesUnaryLong, TR::Int64, rc);

   compileOpCodeMethod(_lReturn, _numberOfUnaryArgs, TR::lreturn, "lReturn", _argTypesUnaryLong, TR::Int64, rc);
   compileOpCodeMethod(_dReturn, _numberOfUnaryArgs, TR::dreturn, "dReturn", _argTypesUnaryDouble, TR::Double, rc);
   compileOpCodeMethod(_fReturn, _numberOfUnaryArgs, TR::freturn, "fReturn", _argTypesUnaryFloat, TR::Float, rc);

   compileOpCodeMethod(_iu2d, _numberOfUnaryArgs, TR::iu2d, "iU2d", _argTypesUnaryInt, TR::Double, rc);
   compileOpCodeMethod(_iu2l, _numberOfUnaryArgs, TR::iu2l, "iu2l", _argTypesUnaryInt, TR::Int64, rc);
   compileOpCodeMethod(_l2f, _numberOfUnaryArgs, TR::l2f, "l2f", _argTypesUnaryLong, TR::Float, rc);
   compileOpCodeMethod(_l2d, _numberOfUnaryArgs, TR::l2d, "l2d", _argTypesUnaryLong, TR::Double, rc);
   compileOpCodeMethod(_f2l, _numberOfUnaryArgs, TR::f2l, "f2l", _argTypesUnaryFloat, TR::Int64, rc);
   compileOpCodeMethod(_f2d, _numberOfUnaryArgs, TR::f2d, "f2d", _argTypesUnaryFloat, TR::Double, rc);
   compileOpCodeMethod(_d2f, _numberOfUnaryArgs, TR::d2f, "d2f", _argTypesUnaryDouble, TR::Float, rc);
   compileOpCodeMethod(_d2l, _numberOfUnaryArgs, TR::d2l, "d2l", _argTypesUnaryDouble, TR::Int64, rc);
   compileOpCodeMethod(_d2b, _numberOfUnaryArgs, TR::d2b, "d2b", _argTypesUnaryDouble, TR::Int8, rc);
   compileOpCodeMethod(_d2s, _numberOfUnaryArgs, TR::d2s, "d2s", _argTypesUnaryDouble, TR::Int16, rc);
   compileOpCodeMethod(_f2b, _numberOfUnaryArgs, TR::f2b, "f2b", _argTypesUnaryFloat, TR::Int8, rc);
   compileOpCodeMethod(_f2s, _numberOfUnaryArgs, TR::f2s, "f2s", _argTypesUnaryFloat, TR::Int16, rc);
   }

void
S390OpCodesTest::compileBitwiseTestMethods()
   {
   int32_t rc;

   compileOpCodeMethod(_lAnd, _numberOfBinaryArgs, TR::land, "lAnd", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_lOr, _numberOfBinaryArgs, TR::lor, "lOr", _argTypesBinaryLong, TR::Int64, rc);
   compileOpCodeMethod(_lXor, _numberOfBinaryArgs, TR::lxor, "lXor", _argTypesBinaryLong, TR::Int64, rc);

   }

void
S390OpCodesTest::compileTernaryTestMethods()
   {
   int32_t rc = 0;
   compileOpCodeMethod(_lternary, _numberOfTernaryArgs, TR::lternary, "lTernary", _argTypesTernaryLong, TR::Int64, rc);

   }

void
S390OpCodesTest::compileCompareTestMethods()
   {
   int32_t rc = 0;

   //Compare
   compileOpCodeMethod(_lCmpeq, _numberOfBinaryArgs, TR::lcmpeq, "lCmpeq", _argTypesBinaryLong, TR::Int32, rc);
   compileOpCodeMethod(_lCmplt, _numberOfBinaryArgs, TR::lcmplt, "lCmplt", _argTypesBinaryLong, TR::Int32, rc);

   compileOpCodeMethod(_dCmpeq, _numberOfBinaryArgs, TR::dcmpeq, "dCmpeq", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_dCmpne, _numberOfBinaryArgs, TR::dcmpne, "dCmpne", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_dCmpgt, _numberOfBinaryArgs, TR::dcmpgt, "dCmpgt", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_dCmplt, _numberOfBinaryArgs, TR::dcmplt, "dCmplt", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_dCmpge, _numberOfBinaryArgs, TR::dcmpge, "dCmpge", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_dCmple, _numberOfBinaryArgs, TR::dcmple, "dCmple", _argTypesBinaryDouble, TR::Int32, rc);

   compileOpCodeMethod(_fCmpeq, _numberOfBinaryArgs, TR::fcmpeq, "fCmpeq", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_fCmpne, _numberOfBinaryArgs, TR::fcmpne, "fCmpne", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_fCmpgt, _numberOfBinaryArgs, TR::fcmpgt, "fCmpgt", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_fCmplt, _numberOfBinaryArgs, TR::fcmplt, "fCmplt", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_fCmpge, _numberOfBinaryArgs, TR::fcmpge, "fCmpge", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_fCmple, _numberOfBinaryArgs, TR::fcmple, "fCmple", _argTypesBinaryFloat, TR::Int32, rc);

   compileOpCodeMethod(_lCmp, _numberOfBinaryArgs, TR::lcmp, "lCmp", _argTypesBinaryLong, TR::Int32, rc);
   compileOpCodeMethod(_fCmpl, _numberOfBinaryArgs, TR::fcmpl, "fCmpl", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_fCmpg, _numberOfBinaryArgs, TR::fcmpg, "fCmpg", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_dCmpl, _numberOfBinaryArgs, TR::dcmpl, "dCmpl", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_dCmpg, _numberOfBinaryArgs, TR::dcmpg, "dCmpg", _argTypesBinaryDouble, TR::Int32, rc);

   compileOpCodeMethod(_ifLcmpeq, _numberOfBinaryArgs, TR::iflcmpeq, "ifLcmpeq", _argTypesBinaryLong, TR::Int32, rc);
   compileOpCodeMethod(_ifLcmpgt, _numberOfBinaryArgs, TR::iflcmpgt, "ifLcmpgt", _argTypesBinaryLong, TR::Int32, rc);
   compileOpCodeMethod(_ifLcmplt, _numberOfBinaryArgs, TR::iflcmplt, "ifLcmplt", _argTypesBinaryLong, TR::Int32, rc);

   compileOpCodeMethod(_ifDcmpeq, _numberOfBinaryArgs, TR::ifdcmpeq, "ifDcmpeq", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_ifDcmpne, _numberOfBinaryArgs, TR::ifdcmpne, "ifDcmpne", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_ifDcmpgt, _numberOfBinaryArgs, TR::ifdcmpgt, "ifDcmpgt", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_ifDcmplt, _numberOfBinaryArgs, TR::ifdcmplt, "ifDcmplt", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_ifDcmpge, _numberOfBinaryArgs, TR::ifdcmpge, "ifDcmpge", _argTypesBinaryDouble, TR::Int32, rc);
   compileOpCodeMethod(_ifDcmple, _numberOfBinaryArgs, TR::ifdcmple, "ifDcmple", _argTypesBinaryDouble, TR::Int32, rc);

   compileOpCodeMethod(_ifFcmpeq, _numberOfBinaryArgs, TR::iffcmpeq, "ifFcmpeq", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_ifFcmpne, _numberOfBinaryArgs, TR::iffcmpne, "ifFcmpne", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_ifFcmpgt, _numberOfBinaryArgs, TR::iffcmpgt, "ifFcmpgt", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_ifFcmplt, _numberOfBinaryArgs, TR::iffcmplt, "ifFcmplt", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_ifFcmpge, _numberOfBinaryArgs, TR::iffcmpge, "ifFcmpge", _argTypesBinaryFloat, TR::Int32, rc);
   compileOpCodeMethod(_ifFcmple, _numberOfBinaryArgs, TR::iffcmple, "ifFcmple", _argTypesBinaryFloat, TR::Int32, rc);

   }

void
S390OpCodesTest::compileDirectCallTestMethods()
   {

   int32_t rc = 0;
#if !defined(TR_TARGET_64BIT)
   compileDirectCallOpCodeMethod(_iCall, _numberOfUnaryArgs, TR::ireturn, TR::icall, "iCompiledMethod", "iCall", _argTypesUnaryInt, TR::Int32, rc);
   compileDirectCallOpCodeMethod(_lCall, _numberOfUnaryArgs, TR::lreturn, TR::lcall, "lCompiledMethod", "lCall", _argTypesUnaryLong, TR::Int64, rc);
   compileDirectCallOpCodeMethod(_dCall, _numberOfUnaryArgs, TR::dreturn, TR::dcall, "dCompiledMethod", "dCall", _argTypesUnaryDouble, TR::Double, rc);
   compileDirectCallOpCodeMethod(_fCall, _numberOfUnaryArgs, TR::freturn, TR::fcall, "fCompiledMethod", "fCall", _argTypesUnaryFloat, TR::Float, rc);
#endif
   }

void
S390OpCodesTest::compileAddressTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_a2b, _numberOfUnaryArgs, TR::a2b, "a2b", _argTypesUnaryAddress, TR::Int8, rc);
   compileOpCodeMethod(_a2s, _numberOfUnaryArgs, TR::a2s, "a2s", _argTypesUnaryAddress, TR::Int16, rc);
   compileOpCodeMethod(_a2l, _numberOfUnaryArgs, TR::a2l, "a2l", _argTypesUnaryAddress, TR::Int64, rc);
#if !defined(TR_TARGET_64BIT)
   compileDirectCallOpCodeMethod(_acall, _numberOfUnaryArgs, TR::areturn, TR::acall, "aCompiledMethod", "acall", _argTypesUnaryAddress, TR::Address, rc);
   compileOpCodeMethod(_i2a, _numberOfUnaryArgs, TR::i2a, "i2a", _argTypesUnaryInt, TR::Address, rc);
   compileOpCodeMethod(_iu2a, _numberOfUnaryArgs, TR::iu2a, "iu2a", _argTypesUnaryInt, TR::Address, rc);
#endif
   compileOpCodeMethod(_l2a, _numberOfUnaryArgs, TR::l2a, "l2a", _argTypesUnaryLong, TR::Address, rc);
   compileOpCodeMethod(_lu2a, _numberOfUnaryArgs, TR::lu2a, "lu2a", _argTypesUnaryLong, TR::Address, rc);

   compileOpCodeMethod(_acmpeq, _numberOfBinaryArgs, TR::acmpeq, "acmpeq", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_acmpne, _numberOfBinaryArgs, TR::acmpne, "acmpne", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_acmplt, _numberOfBinaryArgs, TR::acmplt, "acmplt", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_acmpge, _numberOfBinaryArgs, TR::acmpge, "acmpge", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_acmple, _numberOfBinaryArgs, TR::acmple, "acmple", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_acmpgt, _numberOfBinaryArgs, TR::acmpgt, "acmpgt", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_ifacmpeq, _numberOfBinaryArgs, TR::ifacmpeq, "ifacmpeq", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_ifacmpne, _numberOfBinaryArgs, TR::ifacmpne, "ifacmpne", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_ifacmplt, _numberOfBinaryArgs, TR::ifacmplt, "ifacmplt", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_ifacmpge, _numberOfBinaryArgs, TR::ifacmpge, "ifacmpge", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_ifacmple, _numberOfBinaryArgs, TR::ifacmple, "ifacmple", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_ifacmpgt, _numberOfBinaryArgs, TR::ifacmpgt, "ifacmpgt", _argTypesBinaryAddress, TR::Int32, rc);
   compileOpCodeMethod(_aternary, _numberOfTernaryArgs, TR::aternary, "aternary", _argTypesTernaryAddress, TR::Address, rc);
   }

void
S390OpCodesTest::invokeIntegerArithmeticTests()
   {
   int32_t rc = 0;
   uint32_t testCaseArrLength = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   signatureCharJJ_J_testMethodType * lBinaryCons = 0;
   unsignedSignatureCharJJ_J_testMethodType *luBinaryCons;

   int64_t longAddArr[][2] =
      {
      LONG_ZERO, LONG_ZERO,
      LONG_NEG, LONG_NEG,
      LONG_MINIMUM, LONG_POS,
      LONG_MAXIMUM, LONG_MAXIMUM,
      LONG_POS, LONG_MINIMUM
      };
   int64_t longSubArr[][2] =
      {
      LONG_MAXIMUM, LONG_MINIMUM,
      LONG_NEG, LONG_POS,
      LONG_POS, LONG_MAXIMUM,
      LONG_ZERO, LONG_NEG,
      LONG_MINIMUM, LONG_ZERO
      };
   int64_t longMulArr[][2] =
      {
      LONG_NEG, LONG_MINIMUM,
      LONG_ZERO, LONG_POS,
      LONG_MINIMUM, LONG_NEG,
      LONG_POS, LONG_ZERO,
      LONG_MAXIMUM, LONG_MINIMUM,
      LONG_MINIMUM, LONG_MAXIMUM
      };
   int64_t longDivArr[][2] =
      {
      LONG_NEG, LONG_MAXIMUM,
      LONG_POS, LONG_POS,
      LONG_MAXIMUM, LONG_NEG,
      LONG_ZERO, LONG_MINIMUM
      };
   int64_t longRemArr[][2] =
      {
      LONG_MINIMUM, LONG_MINIMUM,
      LONG_ZERO, LONG_MAXIMUM,
      LONG_POS, LONG_NEG,
      LONG_MAXIMUM, LONG_POS
      };
   uint64_t ulongDivArr[][2] =
      {
      ULONG_POS, ULONG_MAXIMUM,
      ULONG_MINIMUM, ULONG_MAXIMUM,
      ULONG_POS, ULONG_POS,
      ULONG_MAXIMUM, ULONG_POS
      };
   uint64_t ulongRemArr[][2] =
      {
      ULONG_MINIMUM, ULONG_POS,
      ULONG_MAXIMUM, ULONG_POS,
      ULONG_POS, ULONG_MAXIMUM,
      ULONG_MAXIMUM, ULONG_MAXIMUM
      };

   //ladd
   testCaseArrLength = sizeof(longAddArr) / sizeof(longAddArr[0]);
   for(uint32_t i = 0; i < testCaseArrLength; ++i)
      {
      OMR_CT_EXPECT_EQ(_lAdd, add(longAddArr[i][0], longAddArr[i][1]), _lAdd(longAddArr[i][0], longAddArr[i][1]));

      sprintf(resolvedMethodName, "lAddConst1_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::ladd,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &longAddArr[i][0], 2, &longAddArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, add(longAddArr[i][0], longAddArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lAddConst2_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::ladd,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &longAddArr[i][0]);
      OMR_CT_EXPECT_EQ(lBinaryCons, add(longAddArr[i][0], longAddArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, longAddArr[i][1]));

      sprintf(resolvedMethodName, "lAddConst3_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::ladd,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &longAddArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, add(longAddArr[i][0], longAddArr[i][1]), lBinaryCons(longAddArr[i][0], LONG_PLACEHOLDER_2));
      }

   //lsub
   testCaseArrLength = sizeof(longSubArr) / sizeof(longSubArr[0]);
   for(uint32_t i = 0; i < testCaseArrLength; ++i)
      {
      OMR_CT_EXPECT_EQ(_lSub, sub(longSubArr[i][0], longSubArr[i][1]), _lSub(longSubArr[i][0], longSubArr[i][1]));

      sprintf(resolvedMethodName, "lSubConst1_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lsub,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &longSubArr[i][0], 2, &longSubArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, sub(longSubArr[i][0], longSubArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lSubConst2_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lsub,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &longSubArr[i][0]);
      OMR_CT_EXPECT_EQ(lBinaryCons, sub(longSubArr[i][0], longSubArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, longSubArr[i][1]));

      sprintf(resolvedMethodName, "lSubConst3_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lsub,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &longSubArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, sub(longSubArr[i][0], longSubArr[i][1]), lBinaryCons(longSubArr[i][0], LONG_PLACEHOLDER_2));
      }

   //lmul
   testCaseArrLength = sizeof(longMulArr) / sizeof(longMulArr[0]);
   for(uint32_t i = 0; i < testCaseArrLength; ++i)
      {
      OMR_CT_EXPECT_EQ(_lMul, mul(longMulArr[i][0], longMulArr[i][1]), _lMul(longMulArr[i][0], longMulArr[i][1]));

      sprintf(resolvedMethodName, "lMulConst1_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lmul,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &longMulArr[i][0], 2, &longMulArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, mul(longMulArr[i][0], longMulArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lMulConst2_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lmul,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &longMulArr[i][0]);
      OMR_CT_EXPECT_EQ(lBinaryCons, mul(longMulArr[i][0], longMulArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, longMulArr[i][1]));

      sprintf(resolvedMethodName, "lMulConst3_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lmul,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &longMulArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, mul(longMulArr[i][0], longMulArr[i][1]), lBinaryCons(longMulArr[i][0], LONG_PLACEHOLDER_2));
      }

   // iumulh
   OMR_CT_EXPECT_EQ(_iuMulh, iumulh(UINT_MAXIMUM, UINT_MINIMUM), _iuMulh(UINT_MAXIMUM, UINT_MINIMUM));
   OMR_CT_EXPECT_EQ(_iuMulh, iumulh(UINT_MINIMUM, UINT_POS), _iuMulh(UINT_MINIMUM, UINT_POS));
   OMR_CT_EXPECT_EQ(_iuMulh, iumulh(UINT_POS, UINT_MAXIMUM), _iuMulh(UINT_POS, UINT_MAXIMUM));

   //ldiv
   //TODO: _lDiv(LONG_INT, 0)
   testCaseArrLength = sizeof(longDivArr) / sizeof(longDivArr[0]);
   for(uint32_t i = 0; i < testCaseArrLength; ++i)
      {
      OMR_CT_EXPECT_EQ(_lDiv, div(longDivArr[i][0], longDivArr[i][1]), _lDiv(longDivArr[i][0], longDivArr[i][1]));

      sprintf(resolvedMethodName, "lDivConst1_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::ldiv,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &longDivArr[i][0], 2, &longDivArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, div(longDivArr[i][0], longDivArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lDivConst2_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::ldiv,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &longDivArr[i][0]);
      OMR_CT_EXPECT_EQ(lBinaryCons, div(longDivArr[i][0], longDivArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, longDivArr[i][1]));

      sprintf(resolvedMethodName, "lDivConst3_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::ldiv,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &longDivArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, div(longDivArr[i][0], longDivArr[i][1]), lBinaryCons(longDivArr[i][0], LONG_PLACEHOLDER_2));
      }

   //lrem
   //TODO: _lrem(LONG_INT, 0), _lrem(LONG_NEG, 0),
   testCaseArrLength = sizeof(longRemArr) / sizeof(longRemArr[0]);
   for(uint32_t i = 0; i < testCaseArrLength; ++i)
      {
      OMR_CT_EXPECT_EQ(_lRem, rem(longRemArr[i][0], longRemArr[i][1]), _lRem(longRemArr[i][0], longRemArr[i][1]));

      sprintf(resolvedMethodName, "lRemConst1_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lrem,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &longRemArr[i][0], 2, &longRemArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, rem(longRemArr[i][0], longRemArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lRemConst2_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lrem,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &longRemArr[i][0]);
      OMR_CT_EXPECT_EQ(lBinaryCons, rem(longRemArr[i][0], longRemArr[i][1]), lBinaryCons(LONG_PLACEHOLDER_1, longRemArr[i][1]));

      sprintf(resolvedMethodName, "lRemConst3_Testcase%d", i);
      compileOpCodeMethod(lBinaryCons, _numberOfBinaryArgs, TR::lrem,
            resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &longRemArr[i][1]);
      OMR_CT_EXPECT_EQ(lBinaryCons, rem(longRemArr[i][0], longRemArr[i][1]), lBinaryCons(longRemArr[i][0], LONG_PLACEHOLDER_2));
      }

   //ludiv
   //TODO: _luDiv(ULONG_INT, 0)
   testCaseArrLength = sizeof(ulongDivArr) / sizeof(ulongDivArr[0]);
   for(uint32_t i = 0; i < testCaseArrLength; ++i)
      {
      OMR_CT_EXPECT_EQ(_luDiv, div(ulongDivArr[i][0], ulongDivArr[i][1]), _luDiv(ulongDivArr[i][0], ulongDivArr[i][1]));

      sprintf(resolvedMethodName, "luDivConst1_Testcase%d", i);
      compileOpCodeMethod(luBinaryCons, 
            _numberOfBinaryArgs, TR::ludiv, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &ulongDivArr[i][0], 2, &ulongDivArr[i][1]);
      OMR_CT_EXPECT_EQ(luBinaryCons, div(ulongDivArr[i][0], ulongDivArr[i][1]), luBinaryCons(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "luDivConst2_Testcase%d", i);
      compileOpCodeMethod(luBinaryCons, 
            _numberOfBinaryArgs, TR::ludiv, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &ulongDivArr[i][0]);
      OMR_CT_EXPECT_EQ(luBinaryCons, div(ulongDivArr[i][0], ulongDivArr[i][1]), luBinaryCons(LONG_PLACEHOLDER_1, ulongDivArr[i][1]));

      sprintf(resolvedMethodName, "luDivConst3_Testcase%d", i);
      compileOpCodeMethod(luBinaryCons, 
            _numberOfBinaryArgs, TR::ludiv, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &ulongDivArr[i][1]);
      OMR_CT_EXPECT_EQ(luBinaryCons, div(ulongDivArr[i][0], ulongDivArr[i][1]), luBinaryCons(ulongDivArr[i][0], LONG_PLACEHOLDER_2));
      }

   }

void
S390OpCodesTest::invokeFloatArithmeticTests()
   {

   float floatAddArr[][2] =
      {
      FLOAT_ZERO, FLOAT_ZERO,
      FLOAT_NEG, FLOAT_NEG,
      FLOAT_MINIMUM, FLOAT_POS,
      FLOAT_MAXIMUM, FLOAT_MAXIMUM,
      FLOAT_NEG, FLOAT_MINIMUM
      };
   float floatSubArr[][2] =
      {
      FLOAT_MAXIMUM, FLOAT_MINIMUM,
      FLOAT_NEG, FLOAT_POS,
      FLOAT_POS, FLOAT_MAXIMUM,
      FLOAT_ZERO, FLOAT_POS,
      FLOAT_MINIMUM, FLOAT_ZERO
      };
   float floatMulArr[][2] =
      {
      FLOAT_NEG, FLOAT_MINIMUM,
      FLOAT_ZERO, FLOAT_POS,
      FLOAT_MINIMUM, FLOAT_NEG,
      FLOAT_POS, FLOAT_ZERO,
      FLOAT_MINIMUM, FLOAT_MAXIMUM,
      FLOAT_MAXIMUM, FLOAT_MINIMUM
      };
   float floatDivArr[][2] =
      {
      FLOAT_NEG, FLOAT_MAXIMUM,
      FLOAT_POS, FLOAT_POS,
      FLOAT_MAXIMUM, FLOAT_NEG,
      FLOAT_ZERO, FLOAT_MINIMUM,
      };
   double doubleAddArr[][2] =
      {
      DOUBLE_ZERO, DOUBLE_ZERO,
      DOUBLE_NEG, DOUBLE_NEG,
      DOUBLE_MINIMUM, DOUBLE_POS,
      DOUBLE_MAXIMUM, DOUBLE_MAXIMUM,
      DOUBLE_POS, DOUBLE_MINIMUM
      };
   double doubleSubArr[][2] =
      {
      DOUBLE_MAXIMUM, DOUBLE_MINIMUM,
      DOUBLE_NEG, DOUBLE_POS,
      DOUBLE_POS, DOUBLE_MAXIMUM,
      DOUBLE_ZERO, DOUBLE_NEG,
      DOUBLE_MINIMUM, DOUBLE_ZERO
      };
   double doubleMulArr[][2] =
      {
      DOUBLE_NEG, DOUBLE_MINIMUM,
      DOUBLE_ZERO, DOUBLE_POS,
      DOUBLE_MINIMUM, DOUBLE_NEG,
      DOUBLE_POS, DOUBLE_ZERO,
      DOUBLE_MINIMUM, DOUBLE_MAXIMUM,
      DOUBLE_MAXIMUM, DOUBLE_MINIMUM
      };
   double doubleDivArr[][2] =
      {
      DOUBLE_NEG, DOUBLE_MAXIMUM,
      DOUBLE_POS, DOUBLE_POS,
      DOUBLE_MAXIMUM, DOUBLE_NEG,
      DOUBLE_ZERO, DOUBLE_MINIMUM,
      };

   signatureCharDD_D_testMethodType * dBinaryConst = 0;
   signatureCharFF_F_testMethodType * fBinaryConst = 0;

   int32_t rc = 0;
   uint32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   //fadd
   testCaseNum = sizeof(floatAddArr) / sizeof(floatAddArr[0]);
   for (uint32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_FLOAT_EQ(_fAdd, add(floatAddArr[i][0], floatAddArr[i][1]), _fAdd(floatAddArr[i][0], floatAddArr[i][1]));

      sprintf(resolvedMethodName, "fAddConst1_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fadd,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 4, 1, &floatAddArr[i][0], 2, &floatAddArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, add(floatAddArr[i][0], floatAddArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fAddConst2_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fadd,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 1, &floatAddArr[i][0]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, add(floatAddArr[i][0], floatAddArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, floatAddArr[i][1]));

      sprintf(resolvedMethodName, "fAddConst3_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fadd,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 2, &floatAddArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, add(floatAddArr[i][0], floatAddArr[i][1]), fBinaryConst(floatAddArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   //fsub
   testCaseNum = sizeof(floatSubArr) / sizeof(floatSubArr[0]);
   for (uint32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_FLOAT_EQ(_fSub, sub(floatSubArr[i][0], floatSubArr[i][1]), _fSub(floatSubArr[i][0], floatSubArr[i][1]));

      sprintf(resolvedMethodName, "fSubConst1_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fsub,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 4, 1, &floatSubArr[i][0], 2, &floatSubArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, sub(floatSubArr[i][0], floatSubArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fSubConst2_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fsub,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 1, &floatSubArr[i][0]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, sub(floatSubArr[i][0], floatSubArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, floatSubArr[i][1]));

      sprintf(resolvedMethodName, "fSubConst3_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fsub,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 2, &floatSubArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, sub(floatSubArr[i][0], floatSubArr[i][1]), fBinaryConst(floatSubArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   //fmul
   testCaseNum = sizeof(floatMulArr) / sizeof(floatMulArr[0]);
   for (uint32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_FLOAT_EQ(_fMul, mul(floatMulArr[i][0], floatMulArr[i][1]), _fMul(floatMulArr[i][0], floatMulArr[i][1]));

      sprintf(resolvedMethodName, "fMulConst1_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fmul,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 4, 1, &floatMulArr[i][0], 2, &floatMulArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, mul(floatMulArr[i][0], floatMulArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fMulConst2_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fmul,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 1, &floatMulArr[i][0]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, mul(floatMulArr[i][0], floatMulArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, floatMulArr[i][1]));

      sprintf(resolvedMethodName, "fMulConst3_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fmul,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 2, &floatMulArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, mul(floatMulArr[i][0], floatMulArr[i][1]), fBinaryConst(floatMulArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   //fdiv
   testCaseNum = sizeof(floatDivArr) / sizeof(floatDivArr[0]);
   for (uint32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_FLOAT_EQ(_fDiv, div(floatDivArr[i][0], floatDivArr[i][1]), _fDiv(floatDivArr[i][0], floatDivArr[i][1]));

      sprintf(resolvedMethodName, "fDivConst1_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fdiv,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 4, 1, &floatDivArr[i][0], 2, &floatDivArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, div(floatDivArr[i][0], floatDivArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fDivConst2_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fdiv,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 1, &floatDivArr[i][0]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, div(floatDivArr[i][0], floatDivArr[i][1]), fBinaryConst(FLOAT_PLACEHOLDER_1, floatDivArr[i][1]));

      sprintf(resolvedMethodName, "fDivConst3_Testcase%d", i);
      compileOpCodeMethod(fBinaryConst, _numberOfBinaryArgs, TR::fdiv,
            resolvedMethodName, _argTypesBinaryFloat, TR::Float, rc, 2, 2, &floatDivArr[i][1]);
      OMR_CT_EXPECT_FLOAT_EQ(fBinaryConst, div(floatDivArr[i][0], floatDivArr[i][1]), fBinaryConst(floatDivArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(doubleAddArr) / sizeof(doubleAddArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_DOUBLE_EQ(_dAdd, add(doubleAddArr[i][0], doubleAddArr[i][1]), _dAdd(doubleAddArr[i][0], doubleAddArr[i][1]));

      sprintf(resolvedMethodName, "dAddConst1_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dadd,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 4, 1, &doubleAddArr[i][0], 2, &doubleAddArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, add(doubleAddArr[i][0], doubleAddArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dAddConst2_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dadd,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 1, &doubleAddArr[i][0]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, add(doubleAddArr[i][0], doubleAddArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, doubleAddArr[i][1]));

      sprintf(resolvedMethodName, "dAddConst3_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dadd,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 2, &doubleAddArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, add(doubleAddArr[i][0], doubleAddArr[i][1]), dBinaryConst(doubleAddArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(doubleSubArr) / sizeof(doubleSubArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_DOUBLE_EQ(_dSub, sub(doubleSubArr[i][0], doubleSubArr[i][1]), _dSub(doubleSubArr[i][0], doubleSubArr[i][1]));

      sprintf(resolvedMethodName, "dSubConst1_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dsub,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 4, 1, &doubleSubArr[i][0], 2, &doubleSubArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, sub(doubleSubArr[i][0], doubleSubArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dSubConst2_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dsub,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 1, &doubleSubArr[i][0]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, sub(doubleSubArr[i][0], doubleSubArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, doubleSubArr[i][1]));

      sprintf(resolvedMethodName, "dSubConst3_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dsub,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 2, &doubleSubArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, sub(doubleSubArr[i][0], doubleSubArr[i][1]), dBinaryConst(doubleSubArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(doubleMulArr) / sizeof(doubleMulArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_DOUBLE_EQ(_dMul, mul(doubleMulArr[i][0], doubleMulArr[i][1]), _dMul(doubleMulArr[i][0], doubleMulArr[i][1]));

      sprintf(resolvedMethodName, "dMulConst1_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dmul,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 4, 1, &doubleMulArr[i][0], 2, &doubleMulArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, mul(doubleMulArr[i][0], doubleMulArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dMulConst2_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dmul,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 1, &doubleMulArr[i][0]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, mul(doubleMulArr[i][0], doubleMulArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, doubleMulArr[i][1]));

      sprintf(resolvedMethodName, "dMulConst3_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::dmul,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 2, &doubleMulArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, mul(doubleMulArr[i][0], doubleMulArr[i][1]), dBinaryConst(doubleMulArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(doubleDivArr) / sizeof(doubleDivArr[0]);
   for (int32_t i = 0; i < testCaseNum; i++)
      {
      OMR_CT_EXPECT_DOUBLE_EQ(_dDiv, div(doubleDivArr[i][0], doubleDivArr[i][1]), _dDiv(doubleDivArr[i][0], doubleDivArr[i][1]));

      sprintf(resolvedMethodName, "dDivConst1_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::ddiv,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 4, 1, &doubleDivArr[i][0], 2, &doubleDivArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, div(doubleDivArr[i][0], doubleDivArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dDivConst2_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::ddiv,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 1, &doubleDivArr[i][0]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, div(doubleDivArr[i][0], doubleDivArr[i][1]), dBinaryConst(DOUBLE_PLACEHOLDER_1, doubleDivArr[i][1]));

      sprintf(resolvedMethodName, "dDivConst3_Testcase%d", i);
      compileOpCodeMethod(dBinaryConst, _numberOfBinaryArgs, TR::ddiv,
            resolvedMethodName, _argTypesBinaryDouble, TR::Double, rc, 2, 2, &doubleDivArr[i][1]);
      OMR_CT_EXPECT_DOUBLE_EQ(dBinaryConst, div(doubleDivArr[i][0], doubleDivArr[i][1]), dBinaryConst(doubleDivArr[i][0], DOUBLE_PLACEHOLDER_2));
      }
   }

void
S390OpCodesTest::invokeMemoryOperationTests()
   {
   int32_t rc = 0;

   int32_t intDataArray[] = {INT_NEG, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO};
   int16_t shortDataArray[] = {SHORT_NEG, SHORT_POS, SHORT_MAXIMUM, SHORT_MINIMUM, SHORT_ZERO};
   int8_t byteDataArray[] = {BYTE_NEG, BYTE_POS, BYTE_MAXIMUM, BYTE_MINIMUM, BYTE_ZERO};
   int64_t longDataArray[] = {LONG_NEG, LONG_POS, LONG_MAXIMUM, LONG_MINIMUM, LONG_ZERO};
   float floatDataArray[] = {FLOAT_NEG, FLOAT_POS, FLOAT_MAXIMUM, FLOAT_MINIMUM, FLOAT_ZERO};
   double doubleDataArray[] = {DOUBLE_NEG, DOUBLE_POS, DOUBLE_MAXIMUM, DOUBLE_MINIMUM, DOUBLE_ZERO};
   uintptrj_t addressDataArray[] = {(uintptrj_t)&INT_NEG, (uintptrj_t)&LONG_POS, (uintptrj_t)&BYTE_MAXIMUM, (uintptrj_t)&SHORT_MINIMUM, (uintptrj_t)&FLOAT_ZERO};

   signatureCharJ_J_testMethodType  *lMemCons = 0;
   signatureCharD_D_testMethodType  *dMemCons = 0;
   signatureCharF_F_testMethodType  *fMemCons = 0;

   uint32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   testCaseNum = sizeof(longDataArray) / sizeof(longDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "lStoreConst%d", i + 1);
      OMR_CT_EXPECT_EQ(_lStore, longDataArray[i], _lStore(longDataArray[i]));
      compileOpCodeMethod(lMemCons, _numberOfUnaryArgs, TR::lstore, resolvedMethodName, _argTypesUnaryLong, TR::Int64, rc, 2, 1, &(longDataArray[i]));
      OMR_CT_EXPECT_EQ(lMemCons, longDataArray[i], lMemCons(LONG_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "dStoreConst%d", i + 1);
      OMR_CT_EXPECT_DOUBLE_EQ(_dStore, doubleDataArray[i], _dStore(doubleDataArray[i]));
      compileOpCodeMethod(dMemCons, _numberOfUnaryArgs, TR::dstore, resolvedMethodName, _argTypesUnaryDouble, TR::Double, rc, 2, 1, &(doubleDataArray[i]));
      OMR_CT_EXPECT_DOUBLE_EQ(dMemCons, doubleDataArray[i], dMemCons(DOUBLE_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "fStoreConst%d", i + 1);
      OMR_CT_EXPECT_FLOAT_EQ(_fStore, floatDataArray[i], _fStore(floatDataArray[i]));
      compileOpCodeMethod(fMemCons, _numberOfUnaryArgs, TR::fstore, resolvedMethodName, _argTypesUnaryFloat, TR::Float, rc, 2, 1, &(floatDataArray[i]));
      OMR_CT_EXPECT_FLOAT_EQ(fMemCons, floatDataArray[i], fMemCons(FLOAT_PLACEHOLDER_1));
      }

   //indirect load constant
   testCaseNum = sizeof(intDataArray) / sizeof(intDataArray[0]);
   signatureCharL_I_testMethodType  *iLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "iLoadiConst%d", i + 1);
      uintptrj_t intDataAddress = (uintptrj_t)(&intDataArray[i]);
      compileOpCodeMethod(iLoadiCons, _numberOfUnaryArgs, TR::iloadi, resolvedMethodName, _argTypesUnaryAddress, TR::Int32, rc, 2, 1, &intDataAddress);
      OMR_CT_EXPECT_EQ(iLoadiCons, intDataArray[i], iLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(shortDataArray) / sizeof(shortDataArray[0]);
   signatureCharL_S_testMethodType  *sLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "sLoadiConst%d", i + 1);
      uintptrj_t shortDataAddress = (uintptrj_t)(&shortDataArray[i]);
      compileOpCodeMethod(sLoadiCons, _numberOfUnaryArgs, TR::sloadi, resolvedMethodName, _argTypesUnaryAddress, TR::Int16, rc, 2, 1, &shortDataAddress);
      OMR_CT_EXPECT_EQ(sLoadiCons, shortDataArray[i], sLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(byteDataArray) / sizeof(byteDataArray[0]);
   signatureCharL_B_testMethodType  *bLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "bLoadiConst%d", i + 1);
      uintptrj_t byteDataAddress = (uintptrj_t)(&byteDataArray[i]);
      compileOpCodeMethod(bLoadiCons, _numberOfUnaryArgs, TR::bloadi, resolvedMethodName, _argTypesUnaryAddress, TR::Int8, rc, 2, 1, &byteDataAddress);
      OMR_CT_EXPECT_EQ(bLoadiCons, byteDataArray[i], bLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(longDataArray) / sizeof(longDataArray[0]);
   signatureCharL_J_testMethodType  *lLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "lLoadiConst%d", i + 1);
      uintptrj_t longDataAddress = (uintptrj_t)(&longDataArray[i]);
      compileOpCodeMethod(lLoadiCons, _numberOfUnaryArgs, TR::lloadi, resolvedMethodName, _argTypesUnaryAddress, TR::Int64, rc, 2, 1, &longDataAddress);
      OMR_CT_EXPECT_EQ(lLoadiCons, longDataArray[i], lLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   signatureCharL_D_testMethodType  *dLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "dLoadiConst%d", i + 1);
      uintptrj_t doubleDataAddress = (uintptrj_t)(&doubleDataArray[i]);
      compileOpCodeMethod(dLoadiCons, _numberOfUnaryArgs, TR::dloadi, resolvedMethodName, _argTypesUnaryAddress, TR::Double, rc, 2, 1, &doubleDataAddress);
      OMR_CT_EXPECT_EQ(dLoadiCons, doubleDataArray[i], dLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   signatureCharL_F_testMethodType  *fLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "fLoadiConst%d", i + 1);
      uintptrj_t floatDataAddress = (uintptrj_t)(&floatDataArray[i]);
      compileOpCodeMethod(fLoadiCons, _numberOfUnaryArgs, TR::floadi, resolvedMethodName, _argTypesUnaryAddress, TR::Float, rc, 2, 1, &floatDataAddress);
      OMR_CT_EXPECT_EQ(fLoadiCons, floatDataArray[i], fLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(addressDataArray) / sizeof(addressDataArray[0]);
   signatureCharL_L_testMethodType  *aLoadiCons = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "aLoadiConst%d", i + 1);
      uintptrj_t addressDataAddress = (uintptrj_t)(&addressDataArray[i]);
      compileOpCodeMethod(aLoadiCons, _numberOfUnaryArgs, TR::aloadi, resolvedMethodName, _argTypesUnaryAddress, TR::Address, rc, 2, 1, &addressDataAddress);
      OMR_CT_EXPECT_EQ(aLoadiCons, addressDataArray[i], aLoadiCons(ADDRESS_PLACEHOLDER_1));
      }

   int64_t longStoreDataArray[] = {0, 0, 0, 0, 0};
   int32_t intStoreDataArray[] = {0, 0, 0, 0, 0};
   float floatStoreDataArray[] = {0, 0, 0, 0, 0};
   double doubleStoreDataArray[] = {0, 0, 0, 0, 0};
   uintptrj_t addressStoreDataArray[] = {0, 0, 0, 0, 0};
   testCaseNum = sizeof(intDataArray) / sizeof(intDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      if (_iStorei != NULL)
         {
         _iStorei((uintptrj_t)(&intStoreDataArray[i]) , intDataArray[i]);
         EXPECT_EQ(intDataArray[i], intStoreDataArray[i]);
         }
      if (_lStorei != NULL)
         {
         _lStorei((uintptrj_t)(&longStoreDataArray[i]) , longDataArray[i]);
         EXPECT_EQ(longDataArray[i], longStoreDataArray[i]);
         }
      if (_fStorei != NULL)
         {
         _fStorei((uintptrj_t)(&floatStoreDataArray[i]) , floatDataArray[i]);
         EXPECT_EQ(floatDataArray[i], floatStoreDataArray[i]);
         }
      if (_dStorei != NULL)
         {
         _dStorei((uintptrj_t)(&doubleStoreDataArray[i]) , doubleDataArray[i]);
         EXPECT_EQ(doubleDataArray[i], doubleStoreDataArray[i]);
         }
      if (_aStorei != NULL)
         {
         _aStorei((uintptrj_t)(&addressStoreDataArray[i]) , addressDataArray[i]);
         EXPECT_EQ(addressDataArray[i], addressStoreDataArray[i]);
         }
      }
   }

void
S390OpCodesTest::invokeBitwiseTests()
   {
   int32_t rc = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];
   uint32_t testCaseNum = 0;

   int64_t longAndArr [][2] =
      {
      LONG_MINIMUM, LONG_ZERO,
      LONG_MAXIMUM, LONG_NEG
      };
   int64_t longOrArr [][2] =
      {
      LONG_MAXIMUM, LONG_MAXIMUM,
      LONG_ZERO, LONG_ZERO,
      LONG_NEG, LONG_NEG,
      LONG_POS, LONG_MINIMUM,
      LONG_POS, LONG_ZERO,
      LONG_NEG, LONG_ZERO,
      LONG_ZERO, LONG_MAXIMUM
      };
   int64_t longXorArr [][2] =
      {
      LONG_ZERO, LONG_MINIMUM,
      LONG_POS, LONG_NEG,
      LONG_NEG, LONG_POS,
      LONG_MINIMUM, LONG_MAXIMUM,
      LONG_ZERO, LONG_POS,
      LONG_MINIMUM, LONG_NEG
      };

   signatureCharJJ_J_testMethodType * lBitwiseConst = 0;

   //land
   testCaseNum = sizeof(longAndArr) / sizeof(longAndArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_lAnd, tand(longAndArr[i][0], longAndArr[i][1]), _lAnd(longAndArr[i][0], longAndArr[i][1]));

      sprintf(resolvedMethodName, "lAndConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::land, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &(longAndArr[i][0]), 2, &(longAndArr[i][1]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, tand(longAndArr[i][0], longAndArr[i][1]), lBitwiseConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lAndConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::land, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &(longAndArr[i][0]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, tand(longAndArr[i][0], longAndArr[i][1]), lBitwiseConst(LONG_PLACEHOLDER_1, longAndArr[i][1]));

      sprintf(resolvedMethodName, "lAndConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::land, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &(longAndArr[i][1]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, tand(longAndArr[i][0], longAndArr[i][1]), lBitwiseConst(longAndArr[i][0], LONG_PLACEHOLDER_2));
     }

   //lor
   testCaseNum = sizeof(longOrArr) / sizeof(longOrArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_lOr, tor(longOrArr[i][0], longOrArr[i][1]), _lOr(longOrArr[i][0], longOrArr[i][1]));

      sprintf(resolvedMethodName, "lOrConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::lor, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &(longOrArr[i][0]), 2, &(longOrArr[i][1]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, tor(longOrArr[i][0], longOrArr[i][1]), lBitwiseConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lOrConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::lor, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &(longOrArr[i][0]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, tor(longOrArr[i][0], longOrArr[i][1]), lBitwiseConst(LONG_PLACEHOLDER_1, longOrArr[i][1]));

      sprintf(resolvedMethodName, "lOrConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::lor, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &(longOrArr[i][1]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, tor(longOrArr[i][0], longOrArr[i][1]), lBitwiseConst(longOrArr[i][0], LONG_PLACEHOLDER_2));
     }

   //lxor
   testCaseNum = sizeof(longXorArr) / sizeof(longXorArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_lXor, txor(longXorArr[i][0], longXorArr[i][1]), _lXor(longXorArr[i][0], longXorArr[i][1]));

      sprintf(resolvedMethodName, "lXorConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::lxor, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 4, 1, &(longXorArr[i][0]), 2, &(longXorArr[i][1]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, txor(longXorArr[i][0], longXorArr[i][1]), lBitwiseConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lXorConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::lxor, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 1, &(longXorArr[i][0]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, txor(longXorArr[i][0], longXorArr[i][1]), lBitwiseConst(LONG_PLACEHOLDER_1, longXorArr[i][1]));

      sprintf(resolvedMethodName, "lXorConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lBitwiseConst, 
            _numberOfBinaryArgs, TR::lxor, resolvedMethodName, _argTypesBinaryLong, TR::Int64, rc, 2, 2, &(longXorArr[i][1]));
      OMR_CT_EXPECT_EQ(lBitwiseConst, txor(longXorArr[i][0], longXorArr[i][1]), lBitwiseConst(longXorArr[i][0], LONG_PLACEHOLDER_2));
     }
   }

void
S390OpCodesTest::invokeTernaryTests()
   {
   int32_t rc = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];
   uint32_t testCaseNum = 0;
   uint32_t testCaseNumCheck = 0;

   int32_t lternaryChild1Arr[] =
      {
      INT_ZERO,INT_MINIMUM,INT_NEG,INT_MAXIMUM,INT_POS,INT_MINIMUM,
      INT_MAXIMUM,INT_POS,INT_ZERO,INT_MAXIMUM,INT_MAXIMUM
      };

   int64_t longArr[][2] =
      {
      LONG_MAXIMUM, LONG_MINIMUM,
      LONG_POS, LONG_NEG,
      LONG_NEG, LONG_MAXIMUM,
      LONG_MINIMUM, LONG_NEG,
      LONG_ZERO, LONG_POS,
      LONG_MINIMUM, LONG_POS,
      LONG_POS, LONG_MAXIMUM,
      LONG_POS, LONG_MINIMUM,
      LONG_NEG, LONG_POS,
      LONG_NEG, LONG_ZERO,
      LONG_MAXIMUM, LONG_NEG
      };

   testCaseNum = sizeof(lternaryChild1Arr) / sizeof(lternaryChild1Arr[0]);
   testCaseNumCheck = sizeof(longArr) / sizeof(longArr[0]);
   TR_ASSERT( (testCaseNum > 0) && (testCaseNum == testCaseNumCheck), "There is problem in lternary input array");
   signatureCharIJJ_J_testMethodType * lTernaryConst = 0;
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "lTernaryConst%d", i + 1);
      OMR_CT_EXPECT_EQ(_lternary, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), _lternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 6, 1, &lternaryChild1Arr[i], 2, &longArr[i][0], 3, &longArr[i][1]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2, LONG_PLACEHOLDER_3));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 4, 1, &lternaryChild1Arr[i], 2, &longArr[i][0]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2, longArr[i][1]));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 4, 1, &lternaryChild1Arr[i], 3, &longArr[i][1]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(LONG_PLACEHOLDER_1, longArr[i][0], LONG_PLACEHOLDER_3));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 4, 2, &longArr[i][0], 3, &longArr[i][1]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(lternaryChild1Arr[i], LONG_PLACEHOLDER_2, LONG_PLACEHOLDER_3));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 2, 1, &lternaryChild1Arr[i]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(LONG_PLACEHOLDER_1, longArr[i][0], longArr[i][1]));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 2, 2, &longArr[i][0]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(lternaryChild1Arr[i], LONG_PLACEHOLDER_1, longArr[i][1]));

      compileOpCodeMethod(lTernaryConst, _numberOfTernaryArgs, TR::lternary,
            resolvedMethodName, _argTypesTernaryLong, TR::Int64, rc, 2, 3, &longArr[i][1]);
      OMR_CT_EXPECT_EQ(lTernaryConst, ternary(lternaryChild1Arr[i], longArr[i][0], longArr[i][1]), lTernaryConst(lternaryChild1Arr[i], longArr[i][0], LONG_PLACEHOLDER_1));
      }
   }

void
S390OpCodesTest::invokeCompareTests()
   {

   const float FLOAT_NaN = std::numeric_limits<float>::quiet_NaN();
   const double DOUBLE_NaN = std::numeric_limits<float>::quiet_NaN();

   //Compare op codes data array
   int64_t lCmpeqDataArr[][2] =
         {
         LONG_MAXIMUM, LONG_MINIMUM,
         LONG_MAXIMUM, LONG_MAXIMUM
         };
   int64_t lCmpltDataArr[][2] =
         {
         LONG_ZERO, LONG_MAXIMUM,
         LONG_MAXIMUM, LONG_ZERO
         };
   double dCmpeqDataArr[][2] =
         {
         DOUBLE_NEG, DOUBLE_POS,
         DOUBLE_POS, DOUBLE_POS
         };
   double dCmpneDataArr[][2] =
         {
         DOUBLE_POS, DOUBLE_MINIMUM,
         DOUBLE_POS, DOUBLE_POS
         };
   double dCmpgtDataArr[][2] =
         {
         DOUBLE_ZERO, DOUBLE_POS,
         DOUBLE_POS, DOUBLE_ZERO
         };
   double dCmpltDataArr[][2] =
         {
         DOUBLE_MAXIMUM, DOUBLE_ZERO,
         DOUBLE_ZERO, DOUBLE_MAXIMUM
         };
   double dCmpgeDataArr[][2] =
         {
         DOUBLE_POS, DOUBLE_MAXIMUM,
         DOUBLE_MAXIMUM, DOUBLE_POS
         };
   double dCmpleDataArr[][2] =
         {
         DOUBLE_MINIMUM, DOUBLE_NEG,
         DOUBLE_NEG, DOUBLE_MINIMUM
         };
   float fCmpeqDataArr[][2] =
         {
         FLOAT_ZERO, FLOAT_NEG,
         FLOAT_NEG, FLOAT_NEG
         };
   float fCmpneDataArr[][2] =
         {
         FLOAT_NEG, FLOAT_MAXIMUM,
         FLOAT_POS, FLOAT_POS
         };
   float fCmpgtDataArr[][2] =
         {
         FLOAT_MAXIMUM, FLOAT_ZERO,
         FLOAT_ZERO, FLOAT_MAXIMUM
         };
   float fCmpltDataArr[][2] =
         {
         FLOAT_POS, FLOAT_POS,
         FLOAT_ZERO, FLOAT_MAXIMUM
         };
   float fCmpgeDataArr[][2] =
         {
         FLOAT_MINIMUM, FLOAT_MINIMUM,
         FLOAT_POS, FLOAT_MAXIMUM
         };
   float fCmpleDataArr[][2] =
         {
         FLOAT_MINIMUM, FLOAT_NEG,
         FLOAT_NEG, FLOAT_MINIMUM
         };

   //Compare data array
   int64_t lCmpDataArr [][2] =
         {
         LONG_ZERO, LONG_POS,
         LONG_NEG,  LONG_NEG,
         LONG_POS,  LONG_MINIMUM,
         LONG_MINIMUM, LONG_ZERO,
         LONG_MAXIMUM, LONG_MAXIMUM,
         };

   float fCmplDataArr [][2] =
         {
         FLOAT_MAXIMUM, FLOAT_ZERO,
         FLOAT_NEG,  FLOAT_MINIMUM,
         FLOAT_NEG, FLOAT_ZERO,
         FLOAT_MINIMUM, FLOAT_NEG,
         FLOAT_POS, FLOAT_POS,
         FLOAT_MAXIMUM, FLOAT_NaN,
         FLOAT_NaN, FLOAT_NEG,
         FLOAT_NaN, FLOAT_NaN
         };

   float fCmpgDataArr [][2] =
         {
         FLOAT_POS,  FLOAT_MAXIMUM,
         FLOAT_MINIMUM, FLOAT_POS,
         FLOAT_NEG,  FLOAT_MAXIMUM,
         FLOAT_MAXIMUM, FLOAT_MINIMUM,
         FLOAT_MAXIMUM, FLOAT_POS,
         FLOAT_POS,  FLOAT_NEG,
         FLOAT_POS,  FLOAT_POS,
         FLOAT_NaN, FLOAT_POS ,
         FLOAT_MINIMUM, FLOAT_NaN,
         FLOAT_NaN, FLOAT_NaN
         };

   double dCmplDataArr [][2] =
         {
         DOUBLE_MINIMUM, DOUBLE_MAXIMUM,
         DOUBLE_POS,  DOUBLE_ZERO,
         DOUBLE_ZERO, DOUBLE_MAXIMUM,
         DOUBLE_POS,DOUBLE_POS,
         DOUBLE_NEG,DOUBLE_MINIMUM,
         DOUBLE_MINIMUM, DOUBLE_NaN,
         DOUBLE_NaN, DOUBLE_POS,
         DOUBLE_NaN, DOUBLE_NaN
         };

   double dCmpgDataArr [][2] =
         {
         DOUBLE_ZERO, DOUBLE_MINIMUM,
         DOUBLE_NEG,  DOUBLE_ZERO,
         DOUBLE_MAXIMUM, DOUBLE_NEG,
         DOUBLE_POS,  DOUBLE_POS,
         DOUBLE_NEG,  DOUBLE_POS,
         DOUBLE_MINIMUM, DOUBLE_MINIMUM,
         DOUBLE_ZERO, DOUBLE_ZERO,
         DOUBLE_NaN, DOUBLE_ZERO ,
         DOUBLE_NEG, DOUBLE_NaN,
         DOUBLE_NaN, DOUBLE_NaN
         };

   //CompareAndBranch data array
   int64_t ifLcmpeqDataArr[][2] =
         {
         LONG_MAXIMUM, LONG_MINIMUM,
         LONG_MAXIMUM, LONG_MAXIMUM
         };
   int64_t ifLcmpgtDataArr[][2] =
         {
         LONG_MINIMUM, LONG_ZERO,
         LONG_ZERO, LONG_MINIMUM
         };
   int64_t ifLcmpltDataArr[][2] =
         {
         LONG_ZERO, LONG_MAXIMUM,
         LONG_MAXIMUM, LONG_ZERO
         };
   double ifDcmpeqDataArr[][2] =
         {
         DOUBLE_NEG, DOUBLE_POS,
         DOUBLE_POS, DOUBLE_POS
         };
   double ifDcmpneDataArr[][2] =
         {
         DOUBLE_POS, DOUBLE_MINIMUM,
         DOUBLE_POS, DOUBLE_POS
         };
   double ifDcmpgtDataArr[][2] =
         {
         DOUBLE_ZERO, DOUBLE_POS,
         DOUBLE_POS, DOUBLE_ZERO
         };
   double ifDcmpltDataArr[][2] =
         {
         DOUBLE_MAXIMUM, DOUBLE_ZERO,
         DOUBLE_ZERO, DOUBLE_MAXIMUM
         };
   double ifDcmpgeDataArr[][2] =
         {
         DOUBLE_POS, DOUBLE_MAXIMUM,
         DOUBLE_MAXIMUM, DOUBLE_POS
         };
   double ifDcmpleDataArr[][2] =
         {
         DOUBLE_MINIMUM, DOUBLE_NEG,
         DOUBLE_NEG, DOUBLE_MINIMUM
         };
   float ifFcmpeqDataArr[][2] =
         {
         FLOAT_ZERO, FLOAT_NEG,
         FLOAT_NEG, FLOAT_NEG
         };
   float ifFcmpneDataArr[][2] =
         {
         FLOAT_NEG, FLOAT_MAXIMUM,
         FLOAT_POS, FLOAT_POS
         };
   float ifFcmpgtDataArr[][2] =
         {
         FLOAT_MAXIMUM, FLOAT_ZERO,
         FLOAT_ZERO, FLOAT_MAXIMUM
         };
   float ifFcmpltDataArr[][2] =
         {
         FLOAT_POS, FLOAT_POS,
         FLOAT_ZERO, FLOAT_MAXIMUM
         };
   float ifFcmpgeDataArr[][2] =
         {
         FLOAT_MINIMUM, FLOAT_MINIMUM,
         FLOAT_POS, FLOAT_MAXIMUM
         };
   float ifFcmpleDataArr[][2] =
         {
         FLOAT_MINIMUM, FLOAT_NEG,
         FLOAT_NEG, FLOAT_MINIMUM
         };

   int32_t rc = 0;
   int32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   signatureCharJJ_I_testMethodType * lCompareConst = 0;
   signatureCharDD_I_testMethodType * dCompareConst = 0;
   signatureCharFF_I_testMethodType * fCompareConst = 0;

   //lcmpeq
   testCaseNum = sizeof(lCmpeqDataArr) / sizeof(lCmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_lCmpeq, compareEQ(lCmpeqDataArr[i][0], lCmpeqDataArr[i][1]), _lCmpeq(lCmpeqDataArr[i][0], lCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "lCmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmpeq, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 4, 1, &(lCmpeqDataArr[i][0]), 2, &(lCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareEQ(lCmpeqDataArr[i][0], lCmpeqDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lCmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmpeq, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 1, &(lCmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareEQ(lCmpeqDataArr[i][0], lCmpeqDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, lCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "lCmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmpeq, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 2, &(lCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareEQ(lCmpeqDataArr[i][0], lCmpeqDataArr[i][1]), lCompareConst(lCmpeqDataArr[i][0], LONG_PLACEHOLDER_2));
      }

   //lcmplt
   testCaseNum = sizeof(lCmpltDataArr) / sizeof(lCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_lCmplt, compareLT(lCmpltDataArr[i][0], lCmpltDataArr[i][1]), _lCmplt(lCmpltDataArr[i][0], lCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "lCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmplt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 4, 1, &(lCmpltDataArr[i][0]), 2, &(lCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareLT(lCmpltDataArr[i][0], lCmpltDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmplt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 1, &(lCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareLT(lCmpltDataArr[i][0], lCmpltDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, lCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "lCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmplt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 2, &(lCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareLT(lCmpltDataArr[i][0], lCmpltDataArr[i][1]), lCompareConst(lCmpltDataArr[i][0], LONG_PLACEHOLDER_2));
      }

   //dCompare
   testCaseNum = sizeof(dCmpeqDataArr) / sizeof(dCmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmpeq, compareEQ(dCmpeqDataArr[i][0], dCmpeqDataArr[i][1]), _dCmpeq(dCmpeqDataArr[i][0], dCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpeq, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpeqDataArr[i][0]), 2, &(dCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareEQ(dCmpeqDataArr[i][0], dCmpeqDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dCmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpeq, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareEQ(dCmpeqDataArr[i][0], dCmpeqDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpeq, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareEQ(dCmpeqDataArr[i][0], dCmpeqDataArr[i][1]), dCompareConst(dCmpeqDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(dCmpneDataArr) / sizeof(dCmpneDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmpne, compareNE(dCmpneDataArr[i][0], dCmpneDataArr[i][1]), _dCmpne(dCmpneDataArr[i][0], dCmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpne, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpneDataArr[i][0]), 2, &(dCmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareNE(dCmpneDataArr[i][0], dCmpneDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dCmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpne, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareNE(dCmpneDataArr[i][0], dCmpneDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpne, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareNE(dCmpneDataArr[i][0], dCmpneDataArr[i][1]), dCompareConst(dCmpneDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(dCmpgtDataArr) / sizeof(dCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmpgt, compareGT(dCmpgtDataArr[i][0], dCmpgtDataArr[i][1]), _dCmpgt(dCmpgtDataArr[i][0], dCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpgt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpgtDataArr[i][0]), 2, &(dCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGT(dCmpgtDataArr[i][0], dCmpgtDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpgt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGT(dCmpgtDataArr[i][0], dCmpgtDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpgt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGT(dCmpgtDataArr[i][0], dCmpgtDataArr[i][1]), dCompareConst(dCmpgtDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(dCmpltDataArr) / sizeof(dCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmplt, compareLT(dCmpltDataArr[i][0], dCmpltDataArr[i][1]), _dCmplt(dCmpltDataArr[i][0], dCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmplt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpltDataArr[i][0]), 2, &(dCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLT(dCmpltDataArr[i][0], dCmpltDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmplt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLT(dCmpltDataArr[i][0], dCmpltDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmplt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLT(dCmpltDataArr[i][0], dCmpltDataArr[i][1]), dCompareConst(dCmpltDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(dCmpgeDataArr) / sizeof(dCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmpge, compareGE(dCmpgeDataArr[i][0], dCmpgeDataArr[i][1]), _dCmpge(dCmpgeDataArr[i][0], dCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpge, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpgeDataArr[i][0]), 2, &(dCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGE(dCmpgeDataArr[i][0], dCmpgeDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpge, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGE(dCmpgeDataArr[i][0], dCmpgeDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpge, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGE(dCmpgeDataArr[i][0], dCmpgeDataArr[i][1]), dCompareConst(dCmpgeDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(dCmpleDataArr) / sizeof(dCmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmple, compareLE(dCmpleDataArr[i][0], dCmpleDataArr[i][1]), _dCmple(dCmpleDataArr[i][0], dCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmple, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpleDataArr[i][0]), 2, &(dCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLE(dCmpleDataArr[i][0], dCmpleDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "dCmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmple, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLE(dCmpleDataArr[i][0], dCmpleDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "dCmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmple, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLE(dCmpleDataArr[i][0], dCmpleDataArr[i][1]), dCompareConst(dCmpleDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   //fCompare
   testCaseNum = sizeof(fCmpeqDataArr) / sizeof(fCmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmpeq, compareEQ(fCmpeqDataArr[i][0], fCmpeqDataArr[i][1]), _fCmpeq(fCmpeqDataArr[i][0], fCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpeq, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpeqDataArr[i][0]), 2, &(fCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareEQ(fCmpeqDataArr[i][0], fCmpeqDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fCmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpeq, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareEQ(fCmpeqDataArr[i][0], fCmpeqDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpeq, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareEQ(fCmpeqDataArr[i][0], fCmpeqDataArr[i][1]), fCompareConst(fCmpeqDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(fCmpneDataArr) / sizeof(fCmpneDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmpne, compareNE(fCmpneDataArr[i][0], fCmpneDataArr[i][1]), _fCmpne(fCmpneDataArr[i][0], fCmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpne, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpneDataArr[i][0]), 2, &(fCmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareNE(fCmpneDataArr[i][0], fCmpneDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fCmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpne, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareNE(fCmpneDataArr[i][0], fCmpneDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpne, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareNE(fCmpneDataArr[i][0], fCmpneDataArr[i][1]), fCompareConst(fCmpneDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(fCmpgtDataArr) / sizeof(fCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmpgt, compareGT(fCmpgtDataArr[i][0], fCmpgtDataArr[i][1]), _fCmpgt(fCmpgtDataArr[i][0], fCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpgt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpgtDataArr[i][0]), 2, &(fCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGT(fCmpgtDataArr[i][0], fCmpgtDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpgt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGT(fCmpgtDataArr[i][0], fCmpgtDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpgt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGT(fCmpgtDataArr[i][0], fCmpgtDataArr[i][1]), fCompareConst(fCmpgtDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(fCmpltDataArr) / sizeof(fCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmplt, compareLT(fCmpltDataArr[i][0], fCmpltDataArr[i][1]), _fCmplt(fCmpltDataArr[i][0], fCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmplt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpltDataArr[i][0]), 2, &(fCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLT(fCmpltDataArr[i][0], fCmpltDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmplt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLT(fCmpltDataArr[i][0], fCmpltDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmplt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLT(fCmpltDataArr[i][0], fCmpltDataArr[i][1]), fCompareConst(fCmpltDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(fCmpgeDataArr) / sizeof(fCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmpge, compareGE(fCmpgeDataArr[i][0], fCmpgeDataArr[i][1]), _fCmpge(fCmpgeDataArr[i][0], fCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpge, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpgeDataArr[i][0]), 2, &(fCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGE(fCmpgeDataArr[i][0], fCmpgeDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpge, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGE(fCmpgeDataArr[i][0], fCmpgeDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpge, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGE(fCmpgeDataArr[i][0], fCmpgeDataArr[i][1]), fCompareConst(fCmpgeDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(fCmpleDataArr) / sizeof(fCmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmple, compareLE(fCmpleDataArr[i][0], fCmpleDataArr[i][1]), _fCmple(fCmpleDataArr[i][0], fCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmple, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpleDataArr[i][0]), 2, &(fCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLE(fCmpleDataArr[i][0], fCmpleDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "fCmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmple, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLE(fCmpleDataArr[i][0], fCmpleDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "fCmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmple, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLE(fCmpleDataArr[i][0], fCmpleDataArr[i][1]), fCompareConst(fCmpleDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   //lcmp
   testCaseNum = sizeof(lCmpDataArr) / sizeof(lCmpDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_lCmp, comparel(lCmpDataArr[i][0], lCmpDataArr[i][1]), _lCmp(lCmpDataArr[i][0], lCmpDataArr[i][1]));

      sprintf(resolvedMethodName, "lCmpConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmp, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 4, 1, &(lCmpDataArr[i][0]), 2, &(lCmpDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, comparel(lCmpDataArr[i][0], lCmpDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "lCmpConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmp, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 1, &(lCmpDataArr[i][0]));
      OMR_CT_EXPECT_EQ(lCompareConst, comparel(lCmpDataArr[i][0], lCmpDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, lCmpDataArr[i][1]));

      sprintf(resolvedMethodName, "lCmpConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::lcmp, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 2, &(lCmpDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, comparel(lCmpDataArr[i][0], lCmpDataArr[i][1]), lCompareConst(lCmpDataArr[i][0], LONG_PLACEHOLDER_2));
      }

   //dcmpl
   testCaseNum = sizeof(dCmplDataArr) / sizeof(dCmplDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmpl, comparel(dCmplDataArr[i][0], dCmplDataArr[i][1]), _dCmpl(dCmplDataArr[i][0], dCmplDataArr[i][1])) <<
            dCmplDataArr[i][0] << " : " << dCmplDataArr[i][1];

      sprintf(resolvedMethodName, "dCmplConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpl, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmplDataArr[i][0]), 2, &(dCmplDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, comparel(dCmplDataArr[i][0], dCmplDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2)) <<
            dCmplDataArr[i][0] << " : " << dCmplDataArr[i][1];

      sprintf(resolvedMethodName, "dCmplConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpl, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmplDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, comparel(dCmplDataArr[i][0], dCmplDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmplDataArr[i][1])) <<
            dCmplDataArr[i][0] << " : " << dCmplDataArr[i][1];

      sprintf(resolvedMethodName, "dCmplConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpl, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmplDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, comparel(dCmplDataArr[i][0], dCmplDataArr[i][1]), dCompareConst(dCmplDataArr[i][0], DOUBLE_PLACEHOLDER_2)) <<
            dCmplDataArr[i][0] << " : " << dCmplDataArr[i][1];
      }

   //dcmpg
   testCaseNum = sizeof(dCmpgDataArr) / sizeof(dCmpgDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_dCmpg, compareg(dCmpgDataArr[i][0], dCmpgDataArr[i][1]), _dCmpg(dCmpgDataArr[i][0], dCmpgDataArr[i][1])) <<
            dCmpgDataArr[i][0] << " : " << dCmpgDataArr[i][1];

      sprintf(resolvedMethodName, "dCmpgConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpg, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(dCmpgDataArr[i][0]), 2, &(dCmpgDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareg(dCmpgDataArr[i][0], dCmpgDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2)) <<
            dCmpgDataArr[i][0] << " : " << dCmpgDataArr[i][1];

      sprintf(resolvedMethodName, "dCmpgConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpg, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(dCmpgDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareg(dCmpgDataArr[i][0], dCmpgDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, dCmpgDataArr[i][1])) <<
            dCmpgDataArr[i][0] << " : " << dCmpgDataArr[i][1];

      sprintf(resolvedMethodName, "dCmpgConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::dcmpg, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(dCmpgDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareg(dCmpgDataArr[i][0], dCmpgDataArr[i][1]), dCompareConst(dCmpgDataArr[i][0], DOUBLE_PLACEHOLDER_2)) <<
            dCmpgDataArr[i][0] << " : " << dCmpgDataArr[i][1];
      }

   //fcmpl
   testCaseNum = sizeof(fCmplDataArr) / sizeof(fCmplDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmpl, comparel(fCmplDataArr[i][0], fCmplDataArr[i][1]), _fCmpl(fCmplDataArr[i][0], fCmplDataArr[i][1])) <<
            fCmplDataArr[i][0] << " : " << fCmplDataArr[i][1];

      sprintf(resolvedMethodName, "fCmplConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpl, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmplDataArr[i][0]), 2, &(fCmplDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, comparel(fCmplDataArr[i][0], fCmplDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2)) <<
            fCmplDataArr[i][0] << " : " << fCmplDataArr[i][1];

      sprintf(resolvedMethodName, "fCmplConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpl, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmplDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, comparel(fCmplDataArr[i][0], fCmplDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmplDataArr[i][1])) <<
            fCmplDataArr[i][0] << " : " << fCmplDataArr[i][1];

      sprintf(resolvedMethodName, "fCmplConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpl, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmplDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, comparel(fCmplDataArr[i][0], fCmplDataArr[i][1]), fCompareConst(fCmplDataArr[i][0], FLOAT_PLACEHOLDER_2)) <<
            fCmplDataArr[i][0] << " : " << fCmplDataArr[i][1];
      }

   //fcmpg
   testCaseNum = sizeof(fCmpgDataArr) / sizeof(fCmpgDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_fCmpg, compareg(fCmpgDataArr[i][0], fCmpgDataArr[i][1]), _fCmpg(fCmpgDataArr[i][0], fCmpgDataArr[i][1])) <<
            fCmpgDataArr[i][0] << " : " << fCmpgDataArr[i][1];

      sprintf(resolvedMethodName, "fCmpgConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpg, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(fCmpgDataArr[i][0]), 2, &(fCmpgDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareg(fCmpgDataArr[i][0], fCmpgDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2)) <<
            fCmpgDataArr[i][0] << " : " << fCmpgDataArr[i][1];

      sprintf(resolvedMethodName, "fCmpgConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpg, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(fCmpgDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareg(fCmpgDataArr[i][0], fCmpgDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, fCmpgDataArr[i][1])) <<
            fCmpgDataArr[i][0] << " : " << fCmpgDataArr[i][1];

      sprintf(resolvedMethodName, "fCmpgConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::fcmpg, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(fCmpgDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareg(fCmpgDataArr[i][0], fCmpgDataArr[i][1]), fCompareConst(fCmpgDataArr[i][0], FLOAT_PLACEHOLDER_2)) <<
            fCmpgDataArr[i][0] << " : " << fCmpgDataArr[i][1];
      }

   //iflCompare
   testCaseNum = sizeof(ifLcmpeqDataArr) / sizeof(ifLcmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifLcmpeq, compareEQ(ifLcmpeqDataArr[i][0], ifLcmpeqDataArr[i][1]), _ifLcmpeq(ifLcmpeqDataArr[i][0], ifLcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifLcmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmpeq, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 4, 1, &(ifLcmpeqDataArr[i][0]), 2, &(ifLcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareEQ(ifLcmpeqDataArr[i][0], ifLcmpeqDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifLcmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmpeq, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 1, &(ifLcmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareEQ(ifLcmpeqDataArr[i][0], ifLcmpeqDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, ifLcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifLcmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmpeq, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 2, &(ifLcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareEQ(ifLcmpeqDataArr[i][0], ifLcmpeqDataArr[i][1]), lCompareConst(ifLcmpeqDataArr[i][0], LONG_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifLcmpgtDataArr) / sizeof(ifLcmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifLcmpgt, compareGT(ifLcmpgtDataArr[i][0], ifLcmpgtDataArr[i][1]), _ifLcmpgt(ifLcmpgtDataArr[i][0], ifLcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifLcmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmpgt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 4, 1, &(ifLcmpgtDataArr[i][0]), 2, &(ifLcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareGT(ifLcmpgtDataArr[i][0], ifLcmpgtDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifLcmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmpgt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 1, &(ifLcmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareGT(ifLcmpgtDataArr[i][0], ifLcmpgtDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, ifLcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifLcmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmpgt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 2, &(ifLcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareGT(ifLcmpgtDataArr[i][0], ifLcmpgtDataArr[i][1]), lCompareConst(ifLcmpgtDataArr[i][0], LONG_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifLcmpltDataArr) / sizeof(ifLcmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifLcmplt, compareLT(ifLcmpltDataArr[i][0], ifLcmpltDataArr[i][1]), _ifLcmplt(ifLcmpltDataArr[i][0], ifLcmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifLcmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmplt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 4, 1, &(ifLcmpltDataArr[i][0]), 2, &(ifLcmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareLT(ifLcmpltDataArr[i][0], ifLcmpltDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, LONG_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifLcmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmplt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 1, &(ifLcmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareLT(ifLcmpltDataArr[i][0], ifLcmpltDataArr[i][1]), lCompareConst(LONG_PLACEHOLDER_1, ifLcmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifLcmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(lCompareConst, 
            _numberOfBinaryArgs, TR::iflcmplt, resolvedMethodName, _argTypesBinaryLong, TR::Int32, rc, 2, 2, &(ifLcmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(lCompareConst, compareLT(ifLcmpltDataArr[i][0], ifLcmpltDataArr[i][1]), lCompareConst(ifLcmpltDataArr[i][0], LONG_PLACEHOLDER_2));
      }

   //ifdCompare
   testCaseNum = sizeof(dCmpeqDataArr) / sizeof(ifDcmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifDcmpeq, compareEQ(ifDcmpeqDataArr[i][0], ifDcmpeqDataArr[i][1]), _ifDcmpeq(ifDcmpeqDataArr[i][0], ifDcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpeq, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(ifDcmpeqDataArr[i][0]), 2, &(ifDcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareEQ(ifDcmpeqDataArr[i][0], ifDcmpeqDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifDcmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpeq, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(ifDcmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareEQ(ifDcmpeqDataArr[i][0], ifDcmpeqDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, ifDcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpeq, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(ifDcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareEQ(ifDcmpeqDataArr[i][0], ifDcmpeqDataArr[i][1]), dCompareConst(ifDcmpeqDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifDcmpneDataArr) / sizeof(ifDcmpneDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifDcmpne, compareNE(ifDcmpneDataArr[i][0], ifDcmpneDataArr[i][1]), _ifDcmpne(ifDcmpneDataArr[i][0], ifDcmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpne, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(ifDcmpneDataArr[i][0]), 2, &(ifDcmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareNE(ifDcmpneDataArr[i][0], ifDcmpneDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifDcmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpne, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(ifDcmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareNE(ifDcmpneDataArr[i][0], ifDcmpneDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, ifDcmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpne, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(ifDcmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareNE(ifDcmpneDataArr[i][0], ifDcmpneDataArr[i][1]), dCompareConst(ifDcmpneDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifDcmpgtDataArr) / sizeof(ifDcmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifDcmpgt, compareGT(ifDcmpgtDataArr[i][0], ifDcmpgtDataArr[i][1]), _ifDcmpgt(ifDcmpgtDataArr[i][0], ifDcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpgt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(ifDcmpgtDataArr[i][0]), 2, &(ifDcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGT(ifDcmpgtDataArr[i][0], ifDcmpgtDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifDcmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpgt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(ifDcmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGT(ifDcmpgtDataArr[i][0], ifDcmpgtDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, ifDcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpgt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(ifDcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGT(ifDcmpgtDataArr[i][0], ifDcmpgtDataArr[i][1]), dCompareConst(ifDcmpgtDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifDcmpltDataArr) / sizeof(ifDcmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifDcmplt, compareLT(ifDcmpltDataArr[i][0], ifDcmpltDataArr[i][1]), _ifDcmplt(ifDcmpltDataArr[i][0], ifDcmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmplt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(ifDcmpltDataArr[i][0]), 2, &(ifDcmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLT(ifDcmpltDataArr[i][0], ifDcmpltDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifDcmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmplt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(ifDcmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLT(ifDcmpltDataArr[i][0], ifDcmpltDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, ifDcmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmplt, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(ifDcmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLT(ifDcmpltDataArr[i][0], ifDcmpltDataArr[i][1]), dCompareConst(ifDcmpltDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifDcmpgeDataArr) / sizeof(ifDcmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifDcmpge, compareGE(ifDcmpgeDataArr[i][0], ifDcmpgeDataArr[i][1]), _ifDcmpge(ifDcmpgeDataArr[i][0], ifDcmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpge, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(ifDcmpgeDataArr[i][0]), 2, &(ifDcmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGE(ifDcmpgeDataArr[i][0], ifDcmpgeDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifDcmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpge, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(ifDcmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGE(ifDcmpgeDataArr[i][0], ifDcmpgeDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, ifDcmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmpge, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(ifDcmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareGE(ifDcmpgeDataArr[i][0], ifDcmpgeDataArr[i][1]), dCompareConst(ifDcmpgeDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifDcmpleDataArr) / sizeof(ifDcmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifDcmple, compareLE(ifDcmpleDataArr[i][0], ifDcmpleDataArr[i][1]), _ifDcmple(ifDcmpleDataArr[i][0], ifDcmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmple, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 4, 1, &(ifDcmpleDataArr[i][0]), 2, &(ifDcmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLE(ifDcmpleDataArr[i][0], ifDcmpleDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, DOUBLE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifDcmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmple, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 1, &(ifDcmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLE(ifDcmpleDataArr[i][0], ifDcmpleDataArr[i][1]), dCompareConst(DOUBLE_PLACEHOLDER_1, ifDcmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifDcmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(dCompareConst, 
            _numberOfBinaryArgs, TR::ifdcmple, resolvedMethodName, _argTypesBinaryDouble, TR::Int32, rc, 2, 2, &(ifDcmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(dCompareConst, compareLE(ifDcmpleDataArr[i][0], ifDcmpleDataArr[i][1]), dCompareConst(ifDcmpleDataArr[i][0], DOUBLE_PLACEHOLDER_2));
      }

   //iffCompare
   testCaseNum = sizeof(ifFcmpeqDataArr) / sizeof(ifFcmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifFcmpeq, compareEQ(ifFcmpeqDataArr[i][0], ifFcmpeqDataArr[i][1]), _ifFcmpeq(ifFcmpeqDataArr[i][0], ifFcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpeq, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(ifFcmpeqDataArr[i][0]), 2, &(ifFcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareEQ(ifFcmpeqDataArr[i][0], ifFcmpeqDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifFcmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpeq, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(ifFcmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareEQ(ifFcmpeqDataArr[i][0], ifFcmpeqDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, ifFcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpeq, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(ifFcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareEQ(ifFcmpeqDataArr[i][0], ifFcmpeqDataArr[i][1]), fCompareConst(ifFcmpeqDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifFcmpneDataArr) / sizeof(ifFcmpneDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifFcmpne, compareNE(ifFcmpneDataArr[i][0], ifFcmpneDataArr[i][1]), _ifFcmpne(ifFcmpneDataArr[i][0], ifFcmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpne, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(ifFcmpneDataArr[i][0]), 2, &(ifFcmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareNE(ifFcmpneDataArr[i][0], ifFcmpneDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifFcmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpne, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(ifFcmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareNE(ifFcmpneDataArr[i][0], ifFcmpneDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, ifFcmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpne, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(ifFcmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareNE(ifFcmpneDataArr[i][0], ifFcmpneDataArr[i][1]), fCompareConst(ifFcmpneDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifFcmpgtDataArr) / sizeof(ifFcmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifFcmpgt, compareGT(ifFcmpgtDataArr[i][0], ifFcmpgtDataArr[i][1]), _ifFcmpgt(ifFcmpgtDataArr[i][0], ifFcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpgt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(ifFcmpgtDataArr[i][0]), 2, &(ifFcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGT(ifFcmpgtDataArr[i][0], ifFcmpgtDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifFcmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpgt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(ifFcmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGT(ifFcmpgtDataArr[i][0], ifFcmpgtDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, ifFcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpgt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(ifFcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGT(ifFcmpgtDataArr[i][0], ifFcmpgtDataArr[i][1]), fCompareConst(ifFcmpgtDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifFcmpltDataArr) / sizeof(ifFcmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifFcmplt, compareLT(ifFcmpltDataArr[i][0], ifFcmpltDataArr[i][1]), _ifFcmplt(ifFcmpltDataArr[i][0], ifFcmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmplt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(ifFcmpltDataArr[i][0]), 2, &(ifFcmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLT(ifFcmpltDataArr[i][0], ifFcmpltDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifFcmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmplt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(ifFcmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLT(ifFcmpltDataArr[i][0], ifFcmpltDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, ifFcmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmplt, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(ifFcmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLT(ifFcmpltDataArr[i][0], ifFcmpltDataArr[i][1]), fCompareConst(ifFcmpltDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifFcmpgeDataArr) / sizeof(ifFcmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifFcmpge, compareGE(ifFcmpgeDataArr[i][0], ifFcmpgeDataArr[i][1]), _ifFcmpge(ifFcmpgeDataArr[i][0], ifFcmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpge, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(ifFcmpgeDataArr[i][0]), 2, &(ifFcmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGE(ifFcmpgeDataArr[i][0], ifFcmpgeDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifFcmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpge, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(ifFcmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGE(ifFcmpgeDataArr[i][0], ifFcmpgeDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, ifFcmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmpge, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(ifFcmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareGE(ifFcmpgeDataArr[i][0], ifFcmpgeDataArr[i][1]), fCompareConst(ifFcmpgeDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifFcmpleDataArr) / sizeof(ifFcmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifFcmple, compareLE(ifFcmpleDataArr[i][0], ifFcmpleDataArr[i][1]), _ifFcmple(ifFcmpleDataArr[i][0], ifFcmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmple, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 4, 1, &(ifFcmpleDataArr[i][0]), 2, &(ifFcmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLE(ifFcmpleDataArr[i][0], ifFcmpleDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, FLOAT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifFcmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmple, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 1, &(ifFcmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLE(ifFcmpleDataArr[i][0], ifFcmpleDataArr[i][1]), fCompareConst(FLOAT_PLACEHOLDER_1, ifFcmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifFcmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(fCompareConst, 
            _numberOfBinaryArgs, TR::iffcmple, resolvedMethodName, _argTypesBinaryFloat, TR::Int32, rc, 2, 2, &(ifFcmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(fCompareConst, compareLE(ifFcmpleDataArr[i][0], ifFcmpleDataArr[i][1]), fCompareConst(ifFcmpleDataArr[i][0], FLOAT_PLACEHOLDER_2));
      }

   }

void
S390OpCodesTest::invokeUnaryTests()
   {
   int32_t rc = 0;
   int64_t longDataArray[] = {LONG_NEG, LONG_POS, LONG_MAXIMUM, LONG_MINIMUM, LONG_ZERO};
   int32_t intDataArray[] = {INT_NEG, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO};
   int16_t shortDataArray[] = {SHORT_NEG, SHORT_POS, SHORT_MAXIMUM, SHORT_MINIMUM, SHORT_ZERO};
   int8_t byteDataArray[] = {BYTE_NEG, BYTE_POS, BYTE_MAXIMUM, BYTE_MINIMUM, BYTE_ZERO};
   float floatDataArray[] = {FLOAT_NEG, FLOAT_POS, FLOAT_ZERO, FLOAT_MAXIMUM, FLOAT_MINIMUM};
   double doubleDataArray[] = {DOUBLE_NEG, DOUBLE_POS, DOUBLE_ZERO, DOUBLE_MAXIMUM, DOUBLE_MINIMUM};
   uint32_t uintDataArray[] = {UINT_MAXIMUM, UINT_MINIMUM, UINT_POS};
   uint8_t ubyteDataArray[] = {UBYTE_MAXIMUM, UBYTE_MINIMUM, UBYTE_POS};

   uint32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   signatureCharJ_J_testMethodType  *lUnaryCons = 0;
   signatureCharD_D_testMethodType  *dUnaryCons = 0;
   signatureCharF_F_testMethodType  *fUnaryCons = 0;
   signatureCharJ_F_testMethodType * l2fConst = 0;
   signatureCharJ_D_testMethodType * l2dConst = 0;
   signatureCharF_J_testMethodType * f2lConst = 0;
   signatureCharD_J_testMethodType * d2lConst = 0;
   signatureCharF_D_testMethodType * f2dConst = 0;
   signatureCharD_F_testMethodType * d2fConst = 0;
   signatureCharD_B_testMethodType * d2bConst = 0;
   signatureCharD_S_testMethodType * d2sConst = 0;
   signatureCharF_B_testMethodType * f2bConst = 0;
   signatureCharF_S_testMethodType * f2sConst = 0;
   unsignedSignatureCharI_J_testMethodType * iu2lConst = 0;
   unsignedSignatureCharI_D_testMethodType * iu2dConst = 0;

   //labs
   testCaseNum = sizeof(longDataArray) / sizeof(longDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_lAbs, abs(longDataArray[i]), _lAbs(longDataArray[i]));
      sprintf(resolvedMethodName, "lAbsConst%d", i + 1);
      compileOpCodeMethod(lUnaryCons, _numberOfUnaryArgs, TR::labs,
            resolvedMethodName, _argTypesUnaryLong, TR::Int64, rc, 2, 1, &longDataArray[i]);
      OMR_CT_EXPECT_EQ(lUnaryCons, abs(longDataArray[i]), lUnaryCons(LONG_PLACEHOLDER_1));
      }

   //lReturn
   testCaseNum = sizeof(longDataArray) / sizeof(longDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "lReturnCons%d", i + 1);
      OMR_CT_EXPECT_EQ(_lReturn, longDataArray[i], _lReturn(longDataArray[i]));
      compileOpCodeMethod(lUnaryCons, _numberOfUnaryArgs, TR::lreturn, resolvedMethodName, _argTypesUnaryLong, TR::Int64, rc, 2, 1, &(longDataArray[i]));
      OMR_CT_EXPECT_EQ(lUnaryCons, longDataArray[i], lUnaryCons(LONG_PLACEHOLDER_1));
      }

   //dReturn
   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "dReturnCons%d", i + 1);
      OMR_CT_EXPECT_DOUBLE_EQ(_dReturn, doubleDataArray[i], _dReturn(doubleDataArray[i]));
      compileOpCodeMethod(dUnaryCons, _numberOfUnaryArgs, TR::dreturn, resolvedMethodName, _argTypesUnaryDouble, TR::Double, rc, 2, 1, &(doubleDataArray[i]));
      OMR_CT_EXPECT_DOUBLE_EQ(dUnaryCons, doubleDataArray[i], dUnaryCons(DOUBLE_PLACEHOLDER_1));
      }

   //fReturn
   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "fReturnCons%d", i + 1);
      OMR_CT_EXPECT_FLOAT_EQ(_fReturn, floatDataArray[i], _fReturn(floatDataArray[i]));
      compileOpCodeMethod(fUnaryCons, _numberOfUnaryArgs, TR::freturn, resolvedMethodName, _argTypesUnaryFloat, TR::Float, rc, 2, 1, &(floatDataArray[i]));
      OMR_CT_EXPECT_FLOAT_EQ(fUnaryCons, floatDataArray[i], fUnaryCons(FLOAT_PLACEHOLDER_1));
      }

   //lConst
   testCaseNum = sizeof(longDataArray) / sizeof(longDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "lConst%d", i + 1);
      compileOpCodeMethod(lUnaryCons, _numberOfUnaryArgs, TR::lconst, resolvedMethodName, _argTypesUnaryLong, TR::Int64, rc, 2, 1, &(longDataArray[i]));
      OMR_CT_EXPECT_EQ(lUnaryCons, longDataArray[i], lUnaryCons(LONG_PLACEHOLDER_1));
      }

   //dConst
   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "dConst%d", i + 1);
      compileOpCodeMethod(dUnaryCons, _numberOfUnaryArgs, TR::dconst, resolvedMethodName, _argTypesUnaryDouble, TR::Double, rc, 2, 1, &(doubleDataArray[i]));
      OMR_CT_EXPECT_DOUBLE_EQ(dUnaryCons, doubleDataArray[i], dUnaryCons(DOUBLE_PLACEHOLDER_1));
      }

   //fConst
   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "fConst%d", i + 1);
      compileOpCodeMethod(fUnaryCons, _numberOfUnaryArgs, TR::fconst, resolvedMethodName, _argTypesUnaryFloat, TR::Float, rc, 2, 1, &(floatDataArray[i]));
      OMR_CT_EXPECT_FLOAT_EQ(fUnaryCons, floatDataArray[i], fUnaryCons(FLOAT_PLACEHOLDER_1));
      }

   //l 2 d,f
   testCaseNum = sizeof(longDataArray) / sizeof(longDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_FLOAT_EQ(_l2f, convert(longDataArray[i], FLOAT_POS), _l2f(longDataArray[i]));
      OMR_CT_EXPECT_DOUBLE_EQ(_l2d, convert(longDataArray[i], DOUBLE_POS), _l2d(longDataArray[i]));

      sprintf(resolvedMethodName, "l2fConst%d", i + 1);
      compileOpCodeMethod(l2fConst, _numberOfUnaryArgs, TR::l2f,
            resolvedMethodName, _argTypesUnaryLong, TR::Float, rc, 2, 1, &longDataArray[i]);
      OMR_CT_EXPECT_FLOAT_EQ(l2fConst, convert(longDataArray[i], FLOAT_POS), l2fConst(LONG_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "l2dConst%d", i + 1);
      compileOpCodeMethod(l2dConst, _numberOfUnaryArgs, TR::l2d,
            resolvedMethodName, _argTypesUnaryLong, TR::Double, rc, 2, 1, &longDataArray[i]);
      OMR_CT_EXPECT_DOUBLE_EQ(l2dConst, convert(longDataArray[i], DOUBLE_POS), l2dConst(LONG_PLACEHOLDER_1));
      }

   //iu2l
   testCaseNum = sizeof(uintDataArray) / sizeof(uintDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_iu2l, convert(uintDataArray[i], LONG_POS), _iu2l(uintDataArray[i]));

      sprintf(resolvedMethodName, "iu2lConst%d", i + 1);
      compileOpCodeMethod(iu2lConst, _numberOfUnaryArgs, TR::iu2l,
            resolvedMethodName, _argTypesUnaryInt, TR::Int64, rc, 2, 1, &uintDataArray[i]);
      OMR_CT_EXPECT_EQ(iu2lConst, convert(uintDataArray[i], LONG_POS), iu2lConst(INT_PLACEHOLDER_1));
      }

   //iu2d
   testCaseNum = sizeof(uintDataArray) / sizeof(uintDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_iu2d, convert(uintDataArray[i], DOUBLE_POS), _iu2d(uintDataArray[i]));

      sprintf(resolvedMethodName, "iu2dConst%d", i + 1);
      compileOpCodeMethod(iu2dConst, 
            _numberOfUnaryArgs, TR::iu2d, resolvedMethodName, _argTypesUnaryInt, TR::Double, rc, 2, 1, &uintDataArray[i]);
      OMR_CT_EXPECT_EQ(iu2dConst, convert(uintDataArray[i], DOUBLE_POS), iu2dConst(INT_PLACEHOLDER_1));
      }

   //f2l
   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_f2l, convert(floatDataArray[i], LONG_POS), _f2l(floatDataArray[i]));

      sprintf(resolvedMethodName, "f2lConst%d", i + 1);
      compileOpCodeMethod(f2lConst, _numberOfUnaryArgs, TR::f2l,
            resolvedMethodName, _argTypesUnaryFloat, TR::Int64, rc, 2, 1, &floatDataArray[i]);
      OMR_CT_EXPECT_EQ(f2lConst, convert(floatDataArray[i], LONG_POS), f2lConst(FLOAT_PLACEHOLDER_1));
      }

   //d2l
   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_d2l, convert(doubleDataArray[i], LONG_POS), _d2l(doubleDataArray[i]));

      sprintf(resolvedMethodName, "d2lConst%d", i + 1);
      compileOpCodeMethod(d2lConst, _numberOfUnaryArgs, TR::d2l,
            resolvedMethodName, _argTypesUnaryDouble, TR::Int64, rc, 2, 1, &doubleDataArray[i]);
      OMR_CT_EXPECT_EQ(d2lConst, convert(doubleDataArray[i], LONG_POS), d2lConst(DOUBLE_PLACEHOLDER_1));
      }

   //d2b d2s
   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_d2b, convert(doubleDataArray[i], BYTE_POS), _d2b(doubleDataArray[i]));
      OMR_CT_EXPECT_EQ(_d2s, convert(doubleDataArray[i], SHORT_POS), _d2s(doubleDataArray[i]));
      OMR_CT_EXPECT_FLOAT_EQ(_d2f, convert(doubleDataArray[i], FLOAT_POS), _d2f(doubleDataArray[i]));

      sprintf(resolvedMethodName, "d2bConst%d", i + 1);
      compileOpCodeMethod(d2bConst, _numberOfUnaryArgs, TR::d2b,
            resolvedMethodName, _argTypesUnaryDouble, TR::Int8, rc, 2, 1, &doubleDataArray[i]);
      OMR_CT_EXPECT_EQ(d2bConst, convert(doubleDataArray[i], BYTE_POS), d2bConst(DOUBLE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "d2sConst%d", i + 1);
      compileOpCodeMethod(d2sConst, _numberOfUnaryArgs, TR::d2s,
            resolvedMethodName, _argTypesUnaryDouble, TR::Int16, rc, 2, 1, &doubleDataArray[i]);
      OMR_CT_EXPECT_EQ(d2sConst, convert(doubleDataArray[i], SHORT_POS), d2sConst(DOUBLE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "d2fConst%d", i + 1);
      compileOpCodeMethod(d2fConst, _numberOfUnaryArgs, TR::d2f,
            resolvedMethodName, _argTypesUnaryDouble, TR::Float, rc, 2, 1, &doubleDataArray[i]);
      OMR_CT_EXPECT_FLOAT_EQ(d2fConst, convert(doubleDataArray[i], FLOAT_POS), d2fConst(DOUBLE_PLACEHOLDER_1));
      }

   //f2b f2s
   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_f2b, convert(floatDataArray[i], BYTE_POS), _f2b(floatDataArray[i]));
      OMR_CT_EXPECT_EQ(_f2s, convert(floatDataArray[i], SHORT_POS), _f2s(floatDataArray[i]));
      OMR_CT_EXPECT_DOUBLE_EQ(_f2d, convert(floatDataArray[i], DOUBLE_POS), _f2d(floatDataArray[i]));

      sprintf(resolvedMethodName, "f2bConst%d", i + 1);
      compileOpCodeMethod(f2bConst, _numberOfUnaryArgs, TR::f2b,
            resolvedMethodName, _argTypesUnaryFloat, TR::Int8, rc, 2, 1, &floatDataArray[i]);
      OMR_CT_EXPECT_EQ(f2bConst, convert(floatDataArray[i], BYTE_POS), f2bConst(FLOAT_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "f2sConst%d", i + 1);
      compileOpCodeMethod(f2sConst, _numberOfUnaryArgs, TR::f2s,
            resolvedMethodName, _argTypesUnaryFloat, TR::Int16, rc, 2, 1, &floatDataArray[i]);
      OMR_CT_EXPECT_EQ(f2sConst, convert(floatDataArray[i], SHORT_POS), f2sConst(FLOAT_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "f2dConst%d", i + 1);
      compileOpCodeMethod(f2dConst, _numberOfUnaryArgs, TR::f2d,
            resolvedMethodName, _argTypesUnaryFloat, TR::Double, rc, 2, 1, &floatDataArray[i]);
      OMR_CT_EXPECT_EQ(f2dConst, convert(floatDataArray[i], DOUBLE_POS), f2dConst(FLOAT_PLACEHOLDER_1));
      }
   }


void
S390OpCodesTest::invokeDirectCallTests()
   {
#if !defined(TR_TARGET_64BIT)
   int64_t longDataArray[] = {LONG_NEG, LONG_POS, LONG_MAXIMUM, LONG_MINIMUM, LONG_ZERO};
   int32_t intDataArray[] = {INT_NEG, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO};
   float floatDataArray[] = {FLOAT_NEG, FLOAT_POS, FLOAT_MAXIMUM, FLOAT_MINIMUM, FLOAT_ZERO};

   for (int32_t i = 0; i < 5; i++)
      {
      OMR_CT_EXPECT_EQ(_iCall, intDataArray[i], _iCall(intDataArray[i]));
      OMR_CT_EXPECT_FLOAT_EQ(_fCall, floatDataArray[i], _fCall(floatDataArray[i]));
      OMR_CT_EXPECT_EQ(_lCall, longDataArray[i], _lCall(longDataArray[i]));
      }
#endif
   }
} // namespace TestCompiler

#if defined(TR_TARGET_S390)
//groups by testname
TEST(JITS390OpCodesTest, UnaryTest)
   {
   ::TestCompiler::S390OpCodesTest S390UnaryTest;
   S390UnaryTest.compileUnaryTestMethods();
   S390UnaryTest.invokeUnaryTests();
   }

TEST(JITS390OpCodesTest, IntegerArithmeticTest)
   {
   ::TestCompiler::S390OpCodesTest S390IntegerArithmeticTest;
   S390IntegerArithmeticTest.compileIntegerArithmeticTestMethods();
   S390IntegerArithmeticTest.invokeIntegerArithmeticTests();
   }

TEST(JITS390OpCodesTest, FloatArithmeticTest)
   {
   ::TestCompiler::S390OpCodesTest S390FloatArithmeticTest;
   S390FloatArithmeticTest.compileFloatArithmeticTestMethods();
   S390FloatArithmeticTest.invokeFloatArithmeticTests();
   }

TEST(JITS390OpCodesTest, MemoryOperationTest)
   {
   ::TestCompiler::S390OpCodesTest S390MemoryOperationTest;
   S390MemoryOperationTest.compileMemoryOperationTestMethods();
   S390MemoryOperationTest.invokeMemoryOperationTests();
   }

TEST(JITS390OpCodesTest, BitwiseTest)
   {
   ::TestCompiler::S390OpCodesTest BitwiseTest;
   BitwiseTest.compileBitwiseTestMethods();
   BitwiseTest.invokeBitwiseTests();
   }

TEST(JITS390OpCodesTest, TernaryTest)
   {
   ::TestCompiler::S390OpCodesTest S390TernaryTest;
   S390TernaryTest.compileTernaryTestMethods();
   S390TernaryTest.invokeTernaryTests();
   }

TEST(JITS390OpCodesTest, CompareTest)
   {
   ::TestCompiler::S390OpCodesTest S390CompareTest;
   S390CompareTest.compileCompareTestMethods();
   S390CompareTest.invokeCompareTests();
   }

TEST(JITS390OpCodesTest, DirectCallTest)
   {
   ::TestCompiler::S390OpCodesTest S390DirectCallTest;
   S390DirectCallTest.compileDirectCallTestMethods();
   S390DirectCallTest.invokeDirectCallTests();
   }

TEST(JITS390OpCodesTest, S390AddressTest)
   {
   ::TestCompiler::S390OpCodesTest S390AddressTest;
   S390AddressTest.compileAddressTestMethods();
   S390AddressTest.invokeAddressTests();
   }
#endif //defined(TR_TARGET_S390)
