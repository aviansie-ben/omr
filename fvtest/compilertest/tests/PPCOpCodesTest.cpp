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

#include "tests/PPCOpCodesTest.hpp"

namespace TestCompiler
{

void
PPCOpCodesTest::compileUnaryTestMethods()
   {
   int32_t rc = 0;
   compileOpCodeMethod(_d2b, _numberOfUnaryArgs, TR::d2b, "d2b", _argTypesUnaryDouble, TR::Int8, rc);
   compileOpCodeMethod(_d2s, _numberOfUnaryArgs, TR::d2s, "d2s", _argTypesUnaryDouble, TR::Int16, rc);
   compileOpCodeMethod(_f2b, _numberOfUnaryArgs, TR::f2b, "f2b", _argTypesUnaryFloat, TR::Int8, rc);
   compileOpCodeMethod(_f2s, _numberOfUnaryArgs, TR::f2s, "f2s", _argTypesUnaryFloat, TR::Int16, rc);

   compileOpCodeMethod(_b2i, _numberOfUnaryArgs, TR::b2i, "b2i", _argTypesUnaryByte, TR::Int32, rc);
   compileOpCodeMethod(_b2l, _numberOfUnaryArgs, TR::b2l, "b2l", _argTypesUnaryByte, TR::Int64, rc);
   compileOpCodeMethod(_b2s, _numberOfUnaryArgs, TR::b2s, "b2s", _argTypesUnaryByte, TR::Int16, rc);
   compileOpCodeMethod(_bu2i, _numberOfUnaryArgs, TR::bu2i, "bu2i", _argTypesUnaryByte, TR::Int32, rc);
   compileOpCodeMethod(_bu2l, _numberOfUnaryArgs, TR::bu2l, "bu2l", _argTypesUnaryByte, TR::Int64, rc);
   compileOpCodeMethod(_bu2s, _numberOfUnaryArgs, TR::bu2s, "bu2s", _argTypesUnaryByte, TR::Int16, rc);

   compileOpCodeMethod(_s2i, _numberOfUnaryArgs, TR::s2i, "s2i", _argTypesUnaryShort, TR::Int32, rc);
   compileOpCodeMethod(_s2l, _numberOfUnaryArgs, TR::s2l, "s2l", _argTypesUnaryShort, TR::Int64, rc);
   compileOpCodeMethod(_s2b, _numberOfUnaryArgs, TR::s2b, "s2b", _argTypesUnaryShort, TR::Int8, rc);
   compileOpCodeMethod(_su2i, _numberOfUnaryArgs, TR::su2i, "su2i", _argTypesUnaryShort, TR::Int32, rc);
   compileOpCodeMethod(_su2l, _numberOfUnaryArgs, TR::su2l, "su2l", _argTypesUnaryShort, TR::Int64, rc);
   compileOpCodeMethod(_iByteswap, _numberOfUnaryArgs, TR::ibyteswap, "iByteswap", _argTypesUnaryInt, TR::Int32, rc);

   }

void
PPCOpCodesTest::compileMemoryOperationTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_bLoad, _numberOfUnaryArgs, TR::bload, "bLoad", _argTypesUnaryByte, TR::Int8, rc);
   compileOpCodeMethod(_sLoad, _numberOfUnaryArgs, TR::sload, "sLoad", _argTypesUnaryShort, TR::Int16, rc);

   compileOpCodeMethod(_bStore, _numberOfUnaryArgs, TR::bstore, "bStore", _argTypesUnaryByte, TR::Int8, rc);
   compileOpCodeMethod(_sStore, _numberOfUnaryArgs, TR::sstore, "sStore", _argTypesUnaryShort, TR::Int16, rc);
   compileOpCodeMethod(_lStore, _numberOfUnaryArgs, TR::lstore, "lStore", _argTypesUnaryLong, TR::Int64, rc);
   compileOpCodeMethod(_dStore, _numberOfUnaryArgs, TR::dstore, "dStore", _argTypesUnaryDouble, TR::Double, rc);
   compileOpCodeMethod(_fStore, _numberOfUnaryArgs, TR::fstore, "fStore", _argTypesUnaryFloat, TR::Float, rc);

   compileOpCodeMethod(_iStorei, _numberOfBinaryArgs, TR::istorei, "iStorei", _argTypesBinaryAddressInt, TR::Int32, rc);
   compileOpCodeMethod(_lStorei, _numberOfBinaryArgs, TR::lstorei, "lStorei", _argTypesBinaryAddressLong, TR::Int64, rc);
   compileOpCodeMethod(_dStorei, _numberOfBinaryArgs, TR::dstorei, "dStorei", _argTypesBinaryAddressDouble, TR::Double, rc);
   compileOpCodeMethod(_fStorei, _numberOfBinaryArgs, TR::fstorei, "fStorei", _argTypesBinaryAddressFloat, TR::Float, rc);
   compileOpCodeMethod(_sStorei, _numberOfBinaryArgs, TR::sstorei, "sStorei", _argTypesBinaryAddressShort, TR::Int16, rc);
   compileOpCodeMethod(_aStorei, _numberOfBinaryArgs, TR::astorei, "aStorei", _argTypesBinaryAddressAddress, TR::Address, rc);
   }

void
PPCOpCodesTest::compileTernaryTestMethods()
   {
   int32_t rc = 0;
   compileOpCodeMethod(_bternary, _numberOfTernaryArgs, TR::bternary, "bTernary", _argTypesTernaryByte, TR::Int8, rc);
   compileOpCodeMethod(_sternary, _numberOfTernaryArgs, TR::sternary, "sTernary", _argTypesTernaryShort, TR::Int16, rc);
   }

void
PPCOpCodesTest::compileCompareTestMethods()
   {
   int32_t rc = 0;

   //Compare
   compileOpCodeMethod(_sCmpeq, _numberOfBinaryArgs, TR::scmpeq, "sCmpeq", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_sCmpne, _numberOfBinaryArgs, TR::scmpne, "sCmpne", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_sCmpgt, _numberOfBinaryArgs, TR::scmpgt, "sCmpgt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_sCmplt, _numberOfBinaryArgs, TR::scmplt, "sCmplt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_sCmpge, _numberOfBinaryArgs, TR::scmpge, "sCmpge", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_sCmple, _numberOfBinaryArgs, TR::scmple, "sCmple", _argTypesBinaryShort, TR::Int32, rc);

   compileOpCodeMethod(_bCmpeq, _numberOfBinaryArgs, TR::bcmpeq, "bCmpeq", _argTypesBinaryByte, TR::Int32, rc);
   compileOpCodeMethod(_bCmpgt, _numberOfBinaryArgs, TR::bcmpgt, "bCmpgt", _argTypesBinaryByte, TR::Int32, rc);

   compileOpCodeMethod(_iuCmpge, _numberOfBinaryArgs, TR::iucmpge, "iuCmpge", _argTypesBinaryInt, TR::Int32, rc);

   compileOpCodeMethod(_suCmplt, _numberOfBinaryArgs, TR::sucmplt, "suCmplt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_suCmpge, _numberOfBinaryArgs, TR::sucmpge, "suCmpge", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_suCmpgt, _numberOfBinaryArgs, TR::sucmpgt, "suCmpgt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_suCmple, _numberOfBinaryArgs, TR::sucmple, "suCmple", _argTypesBinaryShort, TR::Int32, rc);

   //CompareAndBranch
   compileOpCodeMethod(_ifScmpeq, _numberOfBinaryArgs, TR::ifscmpeq, "ifScmpeq", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifScmpne, _numberOfBinaryArgs, TR::ifscmpne, "ifScmpne", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifScmpgt, _numberOfBinaryArgs, TR::ifscmpgt, "ifScmpgt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifScmplt, _numberOfBinaryArgs, TR::ifscmplt, "ifScmplt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifScmpge, _numberOfBinaryArgs, TR::ifscmpge, "ifScmpge", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifScmple, _numberOfBinaryArgs, TR::ifscmple, "ifScmple", _argTypesBinaryShort, TR::Int32, rc);

   compileOpCodeMethod(_ifBcmpeq, _numberOfBinaryArgs, TR::ifbcmpeq, "ifBcmpeq", _argTypesBinaryByte, TR::Int32, rc);
   compileOpCodeMethod(_ifBcmpgt, _numberOfBinaryArgs, TR::ifbcmpgt, "ifBcmpgt", _argTypesBinaryByte, TR::Int32, rc);

   compileOpCodeMethod(_ifBuCmplt, _numberOfBinaryArgs, TR::ifbucmplt, "ifBuCmplt", _argTypesBinaryByte, TR::Int32, rc);
   compileOpCodeMethod(_ifBuCmpge, _numberOfBinaryArgs, TR::ifbucmpge, "ifBuCmpge", _argTypesBinaryByte, TR::Int32, rc);
   compileOpCodeMethod(_ifBuCmpgt, _numberOfBinaryArgs, TR::ifbucmpgt, "ifBuCmpgt", _argTypesBinaryByte, TR::Int32, rc);
   compileOpCodeMethod(_ifBuCmple, _numberOfBinaryArgs, TR::ifbucmple, "ifBuCmple", _argTypesBinaryByte, TR::Int32, rc);

   compileOpCodeMethod(_ifSuCmpgt, _numberOfBinaryArgs, TR::ifsucmpgt, "ifSuCmpgt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifSuCmple, _numberOfBinaryArgs, TR::ifsucmple, "ifSuCmple", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifSuCmplt, _numberOfBinaryArgs, TR::ifsucmplt, "ifSuCmplt", _argTypesBinaryShort, TR::Int32, rc);
   compileOpCodeMethod(_ifSuCmpge, _numberOfBinaryArgs, TR::ifsucmpge, "ifSuCmpge", _argTypesBinaryShort, TR::Int32, rc);

   }

void
PPCOpCodesTest::compileAddressTestMethods()
   {
   int32_t rc = 0;

   compileOpCodeMethod(_a2b, _numberOfUnaryArgs, TR::a2b, "a2b", _argTypesUnaryAddress, TR::Int8, rc);
   compileOpCodeMethod(_a2s, _numberOfUnaryArgs, TR::a2s, "a2s", _argTypesUnaryAddress, TR::Int16, rc);
   compileOpCodeMethod(_a2l, _numberOfUnaryArgs, TR::a2l, "a2l", _argTypesUnaryAddress, TR::Int64, rc);
   compileOpCodeMethod(_b2a, _numberOfUnaryArgs, TR::b2a, "b2a", _argTypesUnaryByte, TR::Address, rc);
   compileOpCodeMethod(_s2a, _numberOfUnaryArgs, TR::s2a, "s2a", _argTypesUnaryShort, TR::Address, rc);
   compileOpCodeMethod(_i2a, _numberOfUnaryArgs, TR::i2a, "i2a", _argTypesUnaryInt, TR::Address, rc);
   compileOpCodeMethod(_bu2a, _numberOfUnaryArgs, TR::bu2a, "bu2a", _argTypesUnaryByte, TR::Address, rc);
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
PPCOpCodesTest::compileBitwiseTestMethods()
   {
   int32_t rc;

   compileOpCodeMethod(_sAnd, _numberOfBinaryArgs, TR::sand, "sAnd", _argTypesBinaryShort, TR::Int16, rc);
   compileOpCodeMethod(_sOr, _numberOfBinaryArgs, TR::sor, "sOr", _argTypesBinaryShort, TR::Int16, rc);
   compileOpCodeMethod(_sXor, _numberOfBinaryArgs, TR::sxor, "sXor", _argTypesBinaryShort, TR::Int16, rc);

   compileOpCodeMethod(_bAnd, _numberOfBinaryArgs, TR::band, "bAnd", _argTypesBinaryByte, TR::Int8, rc);
   compileOpCodeMethod(_bOr, _numberOfBinaryArgs, TR::bor, "bOr", _argTypesBinaryByte, TR::Int8, rc);
   compileOpCodeMethod(_bXor, _numberOfBinaryArgs, TR::bxor, "bXor", _argTypesBinaryByte, TR::Int8, rc);
   }

void
PPCOpCodesTest::invokeUnaryTests()
   {
   int16_t shortDataArray[] = {SHORT_NEG, SHORT_POS, SHORT_MAXIMUM, SHORT_MINIMUM, SHORT_ZERO};
   int8_t byteDataArray[] = {BYTE_NEG, BYTE_POS, BYTE_MAXIMUM, BYTE_MINIMUM, BYTE_ZERO};
   float floatDataArray[] = {FLOAT_NEG, FLOAT_POS, FLOAT_ZERO, FLOAT_MAXIMUM, FLOAT_MINIMUM};
   double doubleDataArray[] = {DOUBLE_NEG, DOUBLE_POS, DOUBLE_ZERO, DOUBLE_MAXIMUM, DOUBLE_MINIMUM};
   int32_t intDataArray[] = {INT_NEG, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO};

   uint16_t ushortDataArray[] = {USHORT_POS, USHORT_MAXIMUM, USHORT_MINIMUM};
   uint8_t ubyteDataArray[] = {UBYTE_POS, UBYTE_MAXIMUM, UBYTE_MINIMUM};


   int32_t rc = 0;
   uint32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   signatureCharI_I_testMethodType  *iUnaryCons = 0;
   signatureCharD_D_testMethodType  *dUnaryCons = 0;
   signatureCharF_F_testMethodType  *fUnaryCons = 0;

   signatureCharD_B_testMethodType * d2bConst = 0;
   signatureCharD_S_testMethodType * d2sConst = 0;

   signatureCharF_B_testMethodType * f2bConst = 0;
   signatureCharF_S_testMethodType * f2sConst = 0;

   signatureCharS_I_testMethodType * s2iConst = 0;
   signatureCharS_J_testMethodType * s2lConst = 0;
   signatureCharS_B_testMethodType * s2bConst = 0;
   signatureCharB_I_testMethodType * b2iConst = 0;
   signatureCharB_J_testMethodType * b2lConst = 0;
   signatureCharB_S_testMethodType * b2sConst = 0;
   unsignedSignatureCharS_I_testMethodType * su2iConst = 0;
   unsignedSignatureCharS_J_testMethodType * su2lConst = 0;
   unsignedSignatureCharB_I_testMethodType * bu2iConst = 0;
   unsignedSignatureCharB_J_testMethodType * bu2lConst = 0;
   unsignedSignatureCharB_F_testMethodType * bu2fConst = 0;
   unsignedSignatureCharB_D_testMethodType * bu2dConst = 0;
   unsignedSignatureCharB_S_testMethodType * bu2sConst = 0;

   //Temporarily postpone converting FLOAT_MAXIMUM and FLOAT_MINIMUM to int and long.
   //the behavior is undefined since the truncated value of the min and max numbers
   //cannot be represented in the destination types.?? how about to short and byte??
   //and for ppc?

   //d2b d2s
   testCaseNum = sizeof(doubleDataArray) / sizeof(doubleDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_d2b, convert(doubleDataArray[i], BYTE_POS), _d2b(doubleDataArray[i]));
      OMR_CT_EXPECT_EQ(_d2s, convert(doubleDataArray[i], SHORT_POS), _d2s(doubleDataArray[i]));

      sprintf(resolvedMethodName, "d2bConst%d", i + 1);
      compileOpCodeMethod(d2bConst, _numberOfUnaryArgs, TR::d2b,
            resolvedMethodName, _argTypesUnaryDouble, TR::Int8, rc, 2, 1, &doubleDataArray[i]);
      OMR_CT_EXPECT_EQ(d2bConst, convert(doubleDataArray[i], BYTE_POS), d2bConst(DOUBLE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "d2sConst%d", i + 1);
      compileOpCodeMethod(d2sConst, _numberOfUnaryArgs, TR::d2s,
            resolvedMethodName, _argTypesUnaryDouble, TR::Int16, rc, 2, 1, &doubleDataArray[i]);
      OMR_CT_EXPECT_EQ(d2sConst, convert(doubleDataArray[i], SHORT_POS), d2sConst(DOUBLE_PLACEHOLDER_1));
      }

   //f2b f2s
   testCaseNum = sizeof(floatDataArray) / sizeof(floatDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_f2b, convert(floatDataArray[i], BYTE_POS), _f2b(floatDataArray[i]));
      OMR_CT_EXPECT_EQ(_f2s, convert(floatDataArray[i], SHORT_POS), _f2s(floatDataArray[i]));

      sprintf(resolvedMethodName, "f2bConst%d", i + 1);
      compileOpCodeMethod(f2bConst, _numberOfUnaryArgs, TR::f2b,
            resolvedMethodName, _argTypesUnaryFloat, TR::Int8, rc, 2, 1, &floatDataArray[i]);
      OMR_CT_EXPECT_EQ(f2bConst, convert(floatDataArray[i], BYTE_POS), f2bConst(FLOAT_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "f2sConst%d", i + 1);
      compileOpCodeMethod(f2sConst, _numberOfUnaryArgs, TR::f2s,
            resolvedMethodName, _argTypesUnaryFloat, TR::Int16, rc, 2, 1, &floatDataArray[i]);
      OMR_CT_EXPECT_EQ(f2sConst, convert(floatDataArray[i], SHORT_POS), f2sConst(FLOAT_PLACEHOLDER_1));
      }

   //b 2 i,l,s
   testCaseNum = sizeof(byteDataArray) / sizeof(byteDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_b2s, convert(byteDataArray[i], SHORT_POS), _b2s(byteDataArray[i]));
      OMR_CT_EXPECT_EQ(_b2i, convert(byteDataArray[i], INT_POS), _b2i(byteDataArray[i]));
      OMR_CT_EXPECT_EQ(_b2l, convert(byteDataArray[i], LONG_POS), _b2l(byteDataArray[i]));

      sprintf(resolvedMethodName, "b2sConst%d", i + 1);
      compileOpCodeMethod(b2sConst, _numberOfUnaryArgs, TR::b2s,
            resolvedMethodName, _argTypesUnaryByte, TR::Int16, rc, 2, 1, &byteDataArray[i]);
      OMR_CT_EXPECT_EQ(b2sConst, convert(byteDataArray[i], SHORT_POS), b2sConst(BYTE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "b2iConst%d", i + 1);
      compileOpCodeMethod(b2iConst, _numberOfUnaryArgs, TR::b2i,
            resolvedMethodName, _argTypesUnaryByte, TR::Int32, rc, 2, 1, &byteDataArray[i]);
      OMR_CT_EXPECT_EQ(b2iConst, convert(byteDataArray[i], INT_POS), b2iConst(BYTE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "b2lConst%d", i + 1);
      compileOpCodeMethod(b2lConst, _numberOfUnaryArgs, TR::b2l,
            resolvedMethodName, _argTypesUnaryByte, TR::Int64, rc, 2, 1, &byteDataArray[i]);
      OMR_CT_EXPECT_EQ(b2lConst, convert(byteDataArray[i], LONG_POS), b2lConst(BYTE_PLACEHOLDER_1));
      }

   //bu 2 i,l,s
   testCaseNum = sizeof(ubyteDataArray) / sizeof(ubyteDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_bu2s, convert(ubyteDataArray[i], SHORT_POS), _bu2s(ubyteDataArray[i]));
      OMR_CT_EXPECT_EQ(_bu2i, convert(ubyteDataArray[i], INT_POS), _bu2i(ubyteDataArray[i]));
      OMR_CT_EXPECT_EQ(_bu2l, convert(ubyteDataArray[i], LONG_POS), _bu2l(ubyteDataArray[i]));

      sprintf(resolvedMethodName, "bu2sConst%d", i + 1);
      compileOpCodeMethod(bu2sConst, _numberOfUnaryArgs, TR::bu2s,
            resolvedMethodName, _argTypesUnaryByte, TR::Int16, rc, 2, 1, &ubyteDataArray[i]);
      OMR_CT_EXPECT_EQ(bu2sConst, convert(ubyteDataArray[i], SHORT_POS), bu2sConst(BYTE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "bu2iConst%d", i + 1);
      compileOpCodeMethod(bu2iConst, _numberOfUnaryArgs, TR::bu2i,
            resolvedMethodName, _argTypesUnaryByte, TR::Int32, rc, 2, 1, &ubyteDataArray[i]);
      OMR_CT_EXPECT_EQ(bu2iConst, convert(ubyteDataArray[i], INT_POS), bu2iConst(BYTE_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "bu2lConst%d", i + 1);
      compileOpCodeMethod(bu2lConst, _numberOfUnaryArgs, TR::bu2l,
            resolvedMethodName, _argTypesUnaryByte, TR::Int64, rc, 2, 1, &ubyteDataArray[i]);
      OMR_CT_EXPECT_EQ(bu2lConst, convert(ubyteDataArray[i], LONG_POS), bu2lConst(BYTE_PLACEHOLDER_1));
      }

   //s 2 i,l,b
   testCaseNum = sizeof(shortDataArray) / sizeof(shortDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_s2b, convert(shortDataArray[i], BYTE_POS), _s2b(shortDataArray[i]));
      OMR_CT_EXPECT_EQ(_s2i, convert(shortDataArray[i], INT_POS), _s2i(shortDataArray[i]));
      OMR_CT_EXPECT_EQ(_s2l, convert(shortDataArray[i], LONG_POS), _s2l(shortDataArray[i]));

      sprintf(resolvedMethodName, "s2bConst%d", i + 1);
      compileOpCodeMethod(s2bConst, _numberOfUnaryArgs, TR::s2b,
            resolvedMethodName, _argTypesUnaryShort, TR::Int8, rc, 2, 1, &shortDataArray[i]);
      OMR_CT_EXPECT_EQ(s2bConst, convert(shortDataArray[i], BYTE_POS), s2bConst(SHORT_PLACEHOLDER_1));

      sprintf(resolvedMethodName, "s2iConst%d", i + 1);
      compileOpCodeMethod(s2iConst, _numberOfUnaryArgs, TR::s2i,
            resolvedMethodName, _argTypesUnaryShort, TR::Int32, rc, 2, 1, &shortDataArray[i]);
      OMR_CT_EXPECT_EQ(s2iConst, convert(shortDataArray[i], INT_POS), s2iConst(SHORT_PLACEHOLDER_1));


      sprintf(resolvedMethodName, "s2lConst%d", i + 1);
      compileOpCodeMethod(s2lConst, _numberOfUnaryArgs, TR::s2l,
            resolvedMethodName, _argTypesUnaryShort, TR::Int64, rc, 2, 1, &shortDataArray[i]);
      OMR_CT_EXPECT_EQ(s2lConst, convert(shortDataArray[i], LONG_POS), s2lConst(SHORT_PLACEHOLDER_1));
      }
   //su 2 i,l
   testCaseNum = sizeof(ushortDataArray) / sizeof(ushortDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_su2i, convert(ushortDataArray[i], INT_POS), _su2i(ushortDataArray[i]));
      OMR_CT_EXPECT_EQ(_su2l, convert(ushortDataArray[i], LONG_POS), _su2l(ushortDataArray[i]));

      sprintf(resolvedMethodName, "s2iConst%d", i + 1);
      compileOpCodeMethod(su2iConst, _numberOfUnaryArgs, TR::su2i,
            resolvedMethodName, _argTypesUnaryShort, TR::Int32, rc, 2, 1, &ushortDataArray[i]);
      OMR_CT_EXPECT_EQ(su2iConst, convert(ushortDataArray[i], INT_POS), su2iConst(SHORT_PLACEHOLDER_1));


      sprintf(resolvedMethodName, "s2lConst%d", i + 1);
      compileOpCodeMethod(su2lConst, _numberOfUnaryArgs, TR::su2l,
            resolvedMethodName, _argTypesUnaryShort, TR::Int64, rc, 2, 1, &ushortDataArray[i]);
      OMR_CT_EXPECT_EQ(su2lConst, convert(ushortDataArray[i], LONG_POS), su2lConst(SHORT_PLACEHOLDER_1));
      }
   //ibyteswap
   testCaseNum = sizeof(intDataArray) / sizeof(intDataArray[0]);
   for (uint32_t i = 0; i < testCaseNum; i++)
      {
      OMR_CT_EXPECT_EQ(_iByteswap, byteswap(intDataArray[i]), _iByteswap(intDataArray[i]));
      sprintf(resolvedMethodName, "iByteswapConst%d", i + 1);
      compileOpCodeMethod(iUnaryCons, _numberOfUnaryArgs, TR::ibyteswap,
            resolvedMethodName, _argTypesUnaryInt, TR::Int32, rc, 2, 1, &intDataArray[i]);
      OMR_CT_EXPECT_EQ(iUnaryCons, byteswap(intDataArray[i]), iUnaryCons(INT_PLACEHOLDER_1));
      }

   }

void
PPCOpCodesTest::invokeMemoryOperationTests()
   {
   int32_t rc = 0;
   uint32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   signatureCharS_S_testMethodType  *sMemCons = 0;
   signatureCharB_B_testMethodType  *bMemCons = 0;
   signatureCharJ_J_testMethodType  *lMemCons = 0;
   signatureCharD_D_testMethodType  *dMemCons = 0;
   signatureCharF_F_testMethodType  *fMemCons = 0;

   int16_t shortDataArray[] = {SHORT_NEG, SHORT_POS, SHORT_MAXIMUM, SHORT_MINIMUM, SHORT_ZERO};
   int8_t byteDataArray[] = {BYTE_NEG, BYTE_POS, BYTE_MAXIMUM, BYTE_MINIMUM, BYTE_ZERO};
   int64_t longDataArray[] = {LONG_NEG, LONG_POS, LONG_MAXIMUM, LONG_MINIMUM, LONG_ZERO};
   float floatDataArray[] = {FLOAT_NEG, FLOAT_POS, FLOAT_MAXIMUM, FLOAT_MINIMUM, FLOAT_ZERO};
   double doubleDataArray[] = {DOUBLE_NEG, DOUBLE_POS, DOUBLE_MAXIMUM, DOUBLE_MINIMUM, DOUBLE_ZERO};

   OMR_CT_EXPECT_EQ(_bLoad, BYTE_ZERO, _bLoad(BYTE_ZERO));
   OMR_CT_EXPECT_EQ(_bLoad, BYTE_NEG, _bLoad(BYTE_NEG));
   OMR_CT_EXPECT_EQ(_bLoad, BYTE_POS, _bLoad(BYTE_POS));
   OMR_CT_EXPECT_EQ(_bLoad, BYTE_MAXIMUM, _bLoad(BYTE_MAXIMUM));
   OMR_CT_EXPECT_EQ(_bLoad, BYTE_MINIMUM, _bLoad(BYTE_MINIMUM));

   OMR_CT_EXPECT_EQ(_sLoad, SHORT_ZERO, _sLoad(SHORT_ZERO));
   OMR_CT_EXPECT_EQ(_sLoad, SHORT_NEG, _sLoad(SHORT_NEG));
   OMR_CT_EXPECT_EQ(_sLoad, SHORT_POS, _sLoad(SHORT_POS));
   OMR_CT_EXPECT_EQ(_sLoad, SHORT_MAXIMUM, _sLoad(SHORT_MAXIMUM));
   OMR_CT_EXPECT_EQ(_sLoad, SHORT_MINIMUM, _sLoad(SHORT_MINIMUM));

   //store
   testCaseNum = sizeof(byteDataArray) / sizeof(byteDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "bStoreConst%d", i + 1);
      OMR_CT_EXPECT_EQ(_bStore, byteDataArray[i], _bStore(byteDataArray[i]));
      compileOpCodeMethod(bMemCons, _numberOfUnaryArgs, TR::bstore, resolvedMethodName, _argTypesUnaryByte, TR::Int8, rc, 2, 1, &(byteDataArray[i]));
      OMR_CT_EXPECT_EQ(bMemCons, byteDataArray[i], bMemCons(BYTE_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(shortDataArray) / sizeof(shortDataArray[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      sprintf(resolvedMethodName, "sStoreConst%d", i + 1);
      OMR_CT_EXPECT_EQ(_sStore, shortDataArray[i], _sStore(shortDataArray[i]));
      compileOpCodeMethod(sMemCons, _numberOfUnaryArgs, TR::sstore, resolvedMethodName, _argTypesUnaryShort, TR::Int16, rc, 2, 1, &(shortDataArray[i]));
      OMR_CT_EXPECT_EQ(sMemCons, shortDataArray[i], sMemCons(SHORT_PLACEHOLDER_1));
      }

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

   int32_t intDataArray[] = {INT_NEG, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO};
   int64_t longStoreDataArray[] = {0, 0, 0, 0, 0};
   int32_t intStoreDataArray[] = {0, 0, 0, 0, 0};
   int16_t shortStoreDataArray[] = {0, 0, 0, 0, 0};
   float floatStoreDataArray[] = {0, 0, 0, 0, 0};
   double doubleStoreDataArray[] = {0, 0, 0, 0, 0};

   uintptrj_t addressDataArray[] = {(uintptrj_t)&INT_NEG, (uintptrj_t)&LONG_POS, (uintptrj_t)&BYTE_MAXIMUM, (uintptrj_t)&SHORT_MINIMUM, (uintptrj_t)&FLOAT_ZERO};
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

      if (_sStorei != NULL)
         {
         _sStorei((uintptrj_t)(&shortStoreDataArray[i]) , shortDataArray[i]);
         EXPECT_EQ(shortDataArray[i], shortStoreDataArray[i]);
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

   //load
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

      if (_aStorei != NULL)
         {
         _aStorei((uintptrj_t)(&addressStoreDataArray[i]) , addressDataArray[i]);
         EXPECT_EQ(addressDataArray[i], addressStoreDataArray[i]);
         }
      }

   }

void
PPCOpCodesTest::invokeCompareTests()
   {
   int16_t sCmpeqDataArr[][2] =
         {
         SHORT_NEG, SHORT_POS,
         SHORT_POS, SHORT_POS
         };
   int16_t sCmpneDataArr[][2] =
         {
         SHORT_POS, SHORT_MINIMUM,
         SHORT_POS, SHORT_POS
         };
   int16_t sCmpgtDataArr[][2] =
         {
         SHORT_ZERO, SHORT_POS,
         SHORT_POS, SHORT_ZERO
         };
   int16_t sCmpltDataArr[][2] =
         {
         SHORT_MAXIMUM, SHORT_ZERO,
         SHORT_ZERO, SHORT_MAXIMUM
         };
   int16_t sCmpgeDataArr[][2] =
         {
         SHORT_POS, SHORT_MAXIMUM,
         SHORT_MAXIMUM, SHORT_POS
         };
   int16_t sCmpleDataArr[][2] =
         {
         SHORT_MINIMUM, SHORT_NEG,
         SHORT_NEG, SHORT_MINIMUM
         };
   int8_t bCmpeqDataArr[][2] =
         {
         BYTE_NEG, BYTE_POS,
         BYTE_POS, BYTE_POS
         };
   int8_t bCmpgtDataArr[][2] =
         {
         BYTE_ZERO, BYTE_POS,
         BYTE_POS, BYTE_ZERO
         };
   int16_t ifScmpeqDataArr[][2] =
         {
         SHORT_NEG, SHORT_POS,
         SHORT_POS, SHORT_POS
         };
   int16_t ifScmpneDataArr[][2] =
         {
         SHORT_POS, SHORT_MINIMUM,
         SHORT_POS, SHORT_POS
         };
   int16_t ifScmpgtDataArr[][2] =
         {
         SHORT_ZERO, SHORT_POS,
         SHORT_POS, SHORT_ZERO
         };
   int16_t ifScmpltDataArr[][2] =
         {
         SHORT_MAXIMUM, SHORT_ZERO,
         SHORT_ZERO, SHORT_MAXIMUM
         };
   int16_t ifScmpgeDataArr[][2] =
         {
         SHORT_POS, SHORT_MAXIMUM,
         SHORT_MAXIMUM, SHORT_POS
         };
   int16_t ifScmpleDataArr[][2] =
         {
         SHORT_MINIMUM, SHORT_NEG,
         SHORT_NEG, SHORT_MINIMUM
         };
   int8_t ifBcmpeqDataArr[][2] =
         {
         BYTE_NEG, BYTE_POS,
         BYTE_POS, BYTE_POS
         };
   int8_t ifBcmpgtDataArr[][2] =
         {
         BYTE_ZERO, BYTE_POS,
         BYTE_POS, BYTE_ZERO
         };
   uint32_t iuCmpgeDataArr[][2] =
         {
         UINT_MAXIMUM, UINT_MAXIMUM,
         UINT_POS, UINT_MINIMUM,
         UINT_MINIMUM, UINT_POS
         };
   uint8_t ifBuCmpgtDataArr[][2] =
         {
         UBYTE_POS, UBYTE_POS,
         UBYTE_MAXIMUM, UBYTE_POS
         };
   uint8_t ifBuCmpltDataArr[][2] =
         {
         UBYTE_MAXIMUM, UBYTE_MINIMUM,
         UBYTE_MINIMUM, UBYTE_MAXIMUM
         };
   uint8_t ifBuCmpgeDataArr[][2] =
         {
         UBYTE_MINIMUM, UBYTE_MAXIMUM,
         UBYTE_MAXIMUM, UBYTE_MINIMUM
         };
   uint8_t ifBuCmpleDataArr[][2] =
         {
         UBYTE_MINIMUM, UBYTE_POS,
         UBYTE_POS, UBYTE_MINIMUM
         };
   uint16_t suCmpltDataArr[][2] =
         {
         USHORT_MAXIMUM, USHORT_MINIMUM,
         USHORT_MINIMUM, USHORT_MAXIMUM
         };
   uint16_t suCmpleDataArr[][2] =
         {
         USHORT_MAXIMUM, USHORT_POS,
         USHORT_POS, USHORT_MAXIMUM
         };
   uint16_t suCmpgtDataArr[][2] =
         {
         USHORT_MINIMUM, USHORT_MAXIMUM,
         USHORT_MAXIMUM, USHORT_MINIMUM
         };
   uint16_t suCmpgeDataArr[][2] =
         {
         USHORT_MAXIMUM, USHORT_MINIMUM,
         USHORT_MINIMUM, USHORT_MAXIMUM
         };
   uint16_t ifSuCmpgtDataArr[][2] =
         {
         USHORT_POS, USHORT_POS,
         USHORT_MAXIMUM, USHORT_POS
         };
   uint16_t ifSuCmpltDataArr[][2] =
         {
         USHORT_MAXIMUM, USHORT_MINIMUM,
         USHORT_MINIMUM, USHORT_MAXIMUM
         };
   uint16_t ifSuCmpgeDataArr[][2] =
         {
         USHORT_MINIMUM, USHORT_MAXIMUM,
         USHORT_MAXIMUM, USHORT_MINIMUM
         };
   uint16_t ifSuCmpleDataArr[][2] =
         {
         USHORT_MINIMUM, USHORT_POS,
         USHORT_POS, USHORT_MINIMUM
         };

   int32_t rc = 0;
   int32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];

   signatureCharSS_I_testMethodType * sCompareConst = 0;
   signatureCharBB_I_testMethodType * bCompareConst = 0;
   unsignedCompareSignatureCharII_I_testMethodType * iuCompareConst = 0;
   unsignedCompareSignatureCharBB_I_testMethodType * buCompareConst = 0;
   unsignedCompareSignatureCharSS_I_testMethodType * suCompareConst = 0;


   //sCompare
   testCaseNum = sizeof(sCmpeqDataArr) / sizeof(sCmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_sCmpeq, compareEQ(sCmpeqDataArr[i][0], sCmpeqDataArr[i][1]), _sCmpeq(sCmpeqDataArr[i][0], sCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpeq, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(sCmpeqDataArr[i][0]), 2, &(sCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareEQ(sCmpeqDataArr[i][0], sCmpeqDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sCmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpeq, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(sCmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareEQ(sCmpeqDataArr[i][0], sCmpeqDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, sCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpeq, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(sCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareEQ(sCmpeqDataArr[i][0], sCmpeqDataArr[i][1]), sCompareConst(sCmpeqDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(sCmpneDataArr) / sizeof(sCmpneDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_sCmpne, compareNE(sCmpneDataArr[i][0], sCmpneDataArr[i][1]), _sCmpne(sCmpneDataArr[i][0], sCmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpne, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(sCmpneDataArr[i][0]), 2, &(sCmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareNE(sCmpneDataArr[i][0], sCmpneDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sCmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpne, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(sCmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareNE(sCmpneDataArr[i][0], sCmpneDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, sCmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpne, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(sCmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareNE(sCmpneDataArr[i][0], sCmpneDataArr[i][1]), sCompareConst(sCmpneDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(sCmpgtDataArr) / sizeof(sCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_sCmpgt, compareGT(sCmpgtDataArr[i][0], sCmpgtDataArr[i][1]), _sCmpgt(sCmpgtDataArr[i][0], sCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(sCmpgtDataArr[i][0]), 2, &(sCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGT(sCmpgtDataArr[i][0], sCmpgtDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(sCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGT(sCmpgtDataArr[i][0], sCmpgtDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, sCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(sCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGT(sCmpgtDataArr[i][0], sCmpgtDataArr[i][1]), sCompareConst(sCmpgtDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(sCmpltDataArr) / sizeof(sCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_sCmplt, compareLT(sCmpltDataArr[i][0], sCmpltDataArr[i][1]), _sCmplt(sCmpltDataArr[i][0], sCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(sCmpltDataArr[i][0]), 2, &(sCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLT(sCmpltDataArr[i][0], sCmpltDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(sCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLT(sCmpltDataArr[i][0], sCmpltDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, sCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(sCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLT(sCmpltDataArr[i][0], sCmpltDataArr[i][1]), sCompareConst(sCmpltDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(sCmpgeDataArr) / sizeof(sCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_sCmpge, compareGE(sCmpgeDataArr[i][0], sCmpgeDataArr[i][1]), _sCmpge(sCmpgeDataArr[i][0], sCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(sCmpgeDataArr[i][0]), 2, &(sCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGE(sCmpgeDataArr[i][0], sCmpgeDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(sCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGE(sCmpgeDataArr[i][0], sCmpgeDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, sCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(sCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGE(sCmpgeDataArr[i][0], sCmpgeDataArr[i][1]), sCompareConst(sCmpgeDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(sCmpleDataArr) / sizeof(sCmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_sCmple, compareLE(sCmpleDataArr[i][0], sCmpleDataArr[i][1]), _sCmple(sCmpleDataArr[i][0], sCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(sCmpleDataArr[i][0]), 2, &(sCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLE(sCmpleDataArr[i][0], sCmpleDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sCmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(sCmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLE(sCmpleDataArr[i][0], sCmpleDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, sCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "sCmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::scmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(sCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLE(sCmpleDataArr[i][0], sCmpleDataArr[i][1]), sCompareConst(sCmpleDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   //bCompare
   testCaseNum = sizeof(bCmpeqDataArr) / sizeof(bCmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_bCmpeq, compareEQ(bCmpeqDataArr[i][0], bCmpeqDataArr[i][1]), _bCmpeq(bCmpeqDataArr[i][0], bCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "bCmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::bcmpeq, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(bCmpeqDataArr[i][0]), 2, &(bCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareEQ(bCmpeqDataArr[i][0], bCmpeqDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "bCmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::bcmpeq, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(bCmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareEQ(bCmpeqDataArr[i][0], bCmpeqDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, bCmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "bCmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::bcmpeq, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(bCmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareEQ(bCmpeqDataArr[i][0], bCmpeqDataArr[i][1]), bCompareConst(bCmpeqDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(bCmpgtDataArr) / sizeof(bCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_bCmpgt, compareGT(bCmpgtDataArr[i][0], bCmpgtDataArr[i][1]), _bCmpgt(bCmpgtDataArr[i][0], bCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "bCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::bcmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(bCmpgtDataArr[i][0]), 2, &(bCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareGT(bCmpgtDataArr[i][0], bCmpgtDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "bCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::bcmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(bCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareGT(bCmpgtDataArr[i][0], bCmpgtDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, bCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "bCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::bcmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(bCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareGT(bCmpgtDataArr[i][0], bCmpgtDataArr[i][1]), bCompareConst(bCmpgtDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   //ifsCompare
   testCaseNum = sizeof(ifScmpeqDataArr) / sizeof(ifScmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifScmpeq, compareEQ(ifScmpeqDataArr[i][0], ifScmpeqDataArr[i][1]), _ifScmpeq(ifScmpeqDataArr[i][0], ifScmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpeq, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifScmpeqDataArr[i][0]), 2, &(ifScmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareEQ(ifScmpeqDataArr[i][0], ifScmpeqDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifScmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpeq, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifScmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareEQ(ifScmpeqDataArr[i][0], ifScmpeqDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, ifScmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpeq, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifScmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareEQ(ifScmpeqDataArr[i][0], ifScmpeqDataArr[i][1]), sCompareConst(ifScmpeqDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifScmpneDataArr) / sizeof(ifScmpneDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifScmpne, compareNE(ifScmpneDataArr[i][0], ifScmpneDataArr[i][1]), _ifScmpne(ifScmpneDataArr[i][0], ifScmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpne, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifScmpneDataArr[i][0]), 2, &(ifScmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareNE(ifScmpneDataArr[i][0], ifScmpneDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifScmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpne, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifScmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareNE(ifScmpneDataArr[i][0], ifScmpneDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, ifScmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpne, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifScmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareNE(ifScmpneDataArr[i][0], ifScmpneDataArr[i][1]), sCompareConst(ifScmpneDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifScmpgtDataArr) / sizeof(ifScmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifScmpgt, compareGT(ifScmpgtDataArr[i][0], ifScmpgtDataArr[i][1]), _ifScmpgt(ifScmpgtDataArr[i][0], ifScmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifScmpgtDataArr[i][0]), 2, &(ifScmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGT(ifScmpgtDataArr[i][0], ifScmpgtDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifScmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifScmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGT(ifScmpgtDataArr[i][0], ifScmpgtDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, ifScmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifScmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGT(ifScmpgtDataArr[i][0], ifScmpgtDataArr[i][1]), sCompareConst(ifScmpgtDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifScmpltDataArr) / sizeof(ifScmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifScmplt, compareLT(ifScmpltDataArr[i][0], ifScmpltDataArr[i][1]), _ifScmplt(ifScmpltDataArr[i][0], ifScmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifScmpltDataArr[i][0]), 2, &(ifScmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLT(ifScmpltDataArr[i][0], ifScmpltDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifScmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifScmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLT(ifScmpltDataArr[i][0], ifScmpltDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, ifScmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifScmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLT(ifScmpltDataArr[i][0], ifScmpltDataArr[i][1]), sCompareConst(ifScmpltDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifScmpgeDataArr) / sizeof(ifScmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifScmpge, compareGE(ifScmpgeDataArr[i][0], ifScmpgeDataArr[i][1]), _ifScmpge(ifScmpgeDataArr[i][0], ifScmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifScmpgeDataArr[i][0]), 2, &(ifScmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGE(ifScmpgeDataArr[i][0], ifScmpgeDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifScmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifScmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGE(ifScmpgeDataArr[i][0], ifScmpgeDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, ifScmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifScmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareGE(ifScmpgeDataArr[i][0], ifScmpgeDataArr[i][1]), sCompareConst(ifScmpgeDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifScmpleDataArr) / sizeof(ifScmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifScmple, compareLE(ifScmpleDataArr[i][0], ifScmpleDataArr[i][1]), _ifScmple(ifScmpleDataArr[i][0], ifScmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifScmpleDataArr[i][0]), 2, &(ifScmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLE(ifScmpleDataArr[i][0], ifScmpleDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifScmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifScmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLE(ifScmpleDataArr[i][0], ifScmpleDataArr[i][1]), sCompareConst(SHORT_PLACEHOLDER_1, ifScmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifScmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sCompareConst, 
            _numberOfBinaryArgs, TR::ifscmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifScmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(sCompareConst, compareLE(ifScmpleDataArr[i][0], ifScmpleDataArr[i][1]), sCompareConst(ifScmpleDataArr[i][0], SHORT_PLACEHOLDER_2));
      }

   //ifbCompare
   testCaseNum = sizeof(ifBcmpeqDataArr) / sizeof(ifBcmpeqDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifBcmpeq, compareEQ(ifBcmpeqDataArr[i][0], ifBcmpeqDataArr[i][1]), _ifBcmpeq(ifBcmpeqDataArr[i][0], ifBcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBcmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::ifbcmpeq, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(ifBcmpeqDataArr[i][0]), 2, &(ifBcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareEQ(ifBcmpeqDataArr[i][0], ifBcmpeqDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifBcmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::ifbcmpeq, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(ifBcmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareEQ(ifBcmpeqDataArr[i][0], ifBcmpeqDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, ifBcmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBcmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::ifbcmpeq, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(ifBcmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareEQ(ifBcmpeqDataArr[i][0], ifBcmpeqDataArr[i][1]), bCompareConst(ifBcmpeqDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifBcmpgtDataArr) / sizeof(ifBcmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifBcmpgt, compareGT(ifBcmpgtDataArr[i][0], ifBcmpgtDataArr[i][1]), _ifBcmpgt(ifBcmpgtDataArr[i][0], ifBcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBcmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::ifbcmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(ifBcmpgtDataArr[i][0]), 2, &(ifBcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareGT(ifBcmpgtDataArr[i][0], ifBcmpgtDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifBcmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::ifbcmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(ifBcmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareGT(ifBcmpgtDataArr[i][0], ifBcmpgtDataArr[i][1]), bCompareConst(BYTE_PLACEHOLDER_1, ifBcmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBcmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bCompareConst, 
            _numberOfBinaryArgs, TR::ifbcmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(ifBcmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(bCompareConst, compareGT(ifBcmpgtDataArr[i][0], ifBcmpgtDataArr[i][1]), bCompareConst(ifBcmpgtDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(iuCmpgeDataArr) / sizeof(iuCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_iuCmpge, compareGE(iuCmpgeDataArr[i][0], iuCmpgeDataArr[i][1]), _iuCmpge(iuCmpgeDataArr[i][0], iuCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "iuCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(iuCompareConst, 
            _numberOfBinaryArgs, TR::iucmpge, resolvedMethodName, _argTypesBinaryInt, TR::Int32, rc, 4, 1, &(iuCmpgeDataArr[i][0]), 2, &(iuCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(iuCompareConst, compareGE(iuCmpgeDataArr[i][0], iuCmpgeDataArr[i][1]), iuCompareConst(INT_PLACEHOLDER_1, INT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "iuCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(iuCompareConst, 
            _numberOfBinaryArgs, TR::iucmpge, resolvedMethodName, _argTypesBinaryInt, TR::Int32, rc, 2, 1, &(iuCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(iuCompareConst, compareGE(iuCmpgeDataArr[i][0], iuCmpgeDataArr[i][1]), iuCompareConst(INT_PLACEHOLDER_1, iuCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "iuCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(iuCompareConst, 
            _numberOfBinaryArgs, TR::iucmpge, resolvedMethodName, _argTypesBinaryInt, TR::Int32, rc, 2, 2, &(iuCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(iuCompareConst, compareGE(iuCmpgeDataArr[i][0], iuCmpgeDataArr[i][1]), iuCompareConst(iuCmpgeDataArr[i][0], INT_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(suCmpgtDataArr) / sizeof(suCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_suCmpgt, compareGT(suCmpgtDataArr[i][0], suCmpgtDataArr[i][1]), _suCmpgt(suCmpgtDataArr[i][0], suCmpgtDataArr[i][1])) << suCmpgtDataArr[i][0] << " : " << suCmpgtDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(suCmpgtDataArr[i][0]), 2, &(suCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGT(suCmpgtDataArr[i][0], suCmpgtDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << suCmpgtDataArr[i][0] << " : " << suCmpgtDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(suCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGT(suCmpgtDataArr[i][0], suCmpgtDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, suCmpgtDataArr[i][1])) << suCmpgtDataArr[i][0] << " : " << suCmpgtDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(suCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGT(suCmpgtDataArr[i][0], suCmpgtDataArr[i][1]), suCompareConst(suCmpgtDataArr[i][0], SHORT_PLACEHOLDER_2)) << suCmpgtDataArr[i][0] << " : " << suCmpgtDataArr[i][1];
      }

   testCaseNum = sizeof(suCmpltDataArr) / sizeof(suCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_suCmplt, compareLT(suCmpltDataArr[i][0], suCmpltDataArr[i][1]), _suCmplt(suCmpltDataArr[i][0], suCmpltDataArr[i][1])) << suCmpltDataArr[i][0] << " : " << suCmpltDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(suCmpltDataArr[i][0]), 2, &(suCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLT(suCmpltDataArr[i][0], suCmpltDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << suCmpltDataArr[i][0] << " : " << suCmpltDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(suCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLT(suCmpltDataArr[i][0], suCmpltDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, suCmpltDataArr[i][1])) << suCmpltDataArr[i][0] << " : " << suCmpltDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(suCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLT(suCmpltDataArr[i][0], suCmpltDataArr[i][1]), suCompareConst(suCmpltDataArr[i][0], SHORT_PLACEHOLDER_2)) << suCmpltDataArr[i][0] << " : " << suCmpltDataArr[i][1];
      }

   testCaseNum = sizeof(suCmpgeDataArr) / sizeof(suCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_suCmpge, compareGE(suCmpgeDataArr[i][0], suCmpgeDataArr[i][1]), _suCmpge(suCmpgeDataArr[i][0], suCmpgeDataArr[i][1])) << suCmpgeDataArr[i][0] << " : " << suCmpgeDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(suCmpgeDataArr[i][0]), 2, &(suCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGE(suCmpgeDataArr[i][0], suCmpgeDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << suCmpgeDataArr[i][0] << " : " << suCmpgeDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(suCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGE(suCmpgeDataArr[i][0], suCmpgeDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, suCmpgeDataArr[i][1])) << suCmpgeDataArr[i][0] << " : " << suCmpgeDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(suCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGE(suCmpgeDataArr[i][0], suCmpgeDataArr[i][1]), suCompareConst(suCmpgeDataArr[i][0], SHORT_PLACEHOLDER_2)) << suCmpgeDataArr[i][0] << " : " << suCmpgeDataArr[i][1];
      }

   testCaseNum = sizeof(suCmpleDataArr) / sizeof(suCmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_suCmple, compareLE(suCmpleDataArr[i][0], suCmpleDataArr[i][1]), _suCmple(suCmpleDataArr[i][0], suCmpleDataArr[i][1])) << suCmpleDataArr[i][0] << " : " << suCmpleDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(suCmpleDataArr[i][0]), 2, &(suCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLE(suCmpleDataArr[i][0], suCmpleDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << suCmpleDataArr[i][0] << " : " << suCmpleDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(suCmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLE(suCmpleDataArr[i][0], suCmpleDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, suCmpleDataArr[i][1])) << suCmpleDataArr[i][0] << " : " << suCmpleDataArr[i][1];

      sprintf(resolvedMethodName, "suCmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::sucmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(suCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLE(suCmpleDataArr[i][0], suCmpleDataArr[i][1]), suCompareConst(suCmpleDataArr[i][0], SHORT_PLACEHOLDER_2)) << suCmpleDataArr[i][0] << " : " << suCmpleDataArr[i][1];
      }

   //ifBuCompare
   testCaseNum = sizeof(ifBuCmpgtDataArr) / sizeof(ifBuCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifBuCmpgt, compareGT(ifBuCmpgtDataArr[i][0], ifBuCmpgtDataArr[i][1]), _ifBuCmpgt(ifBuCmpgtDataArr[i][0], ifBuCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(ifBuCmpgtDataArr[i][0]), 2, &(ifBuCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareGT(ifBuCmpgtDataArr[i][0], ifBuCmpgtDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifBuCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(ifBuCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareGT(ifBuCmpgtDataArr[i][0], ifBuCmpgtDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, ifBuCmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmpgt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(ifBuCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareGT(ifBuCmpgtDataArr[i][0], ifBuCmpgtDataArr[i][1]), buCompareConst(ifBuCmpgtDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifBuCmpltDataArr) / sizeof(ifBuCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifBuCmplt, compareLT(ifBuCmpltDataArr[i][0], ifBuCmpltDataArr[i][1]), _ifBuCmplt(ifBuCmpltDataArr[i][0], ifBuCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmplt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(ifBuCmpltDataArr[i][0]), 2, &(ifBuCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareLT(ifBuCmpltDataArr[i][0], ifBuCmpltDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifBuCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmplt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(ifBuCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareLT(ifBuCmpltDataArr[i][0], ifBuCmpltDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, ifBuCmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmplt, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(ifBuCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareLT(ifBuCmpltDataArr[i][0], ifBuCmpltDataArr[i][1]), buCompareConst(ifBuCmpltDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifBuCmpgeDataArr) / sizeof(ifBuCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifBuCmpge, compareGE(ifBuCmpgeDataArr[i][0], ifBuCmpgeDataArr[i][1]), _ifBuCmpge(ifBuCmpgeDataArr[i][0], ifBuCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmpge, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(ifBuCmpgeDataArr[i][0]), 2, &(ifBuCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareGE(ifBuCmpgeDataArr[i][0], ifBuCmpgeDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifBuCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmpge, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(ifBuCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareGE(ifBuCmpgeDataArr[i][0], ifBuCmpgeDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, ifBuCmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmpge, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(ifBuCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareGE(ifBuCmpgeDataArr[i][0], ifBuCmpgeDataArr[i][1]), buCompareConst(ifBuCmpgeDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifBuCmpleDataArr) / sizeof(ifBuCmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifBuCmple, compareLE(ifBuCmpleDataArr[i][0], ifBuCmpleDataArr[i][1]), _ifBuCmple(ifBuCmpleDataArr[i][0], ifBuCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmple, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 4, 1, &(ifBuCmpleDataArr[i][0]), 2, &(ifBuCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareLE(ifBuCmpleDataArr[i][0], ifBuCmpleDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifBuCmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmple, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 1, &(ifBuCmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareLE(ifBuCmpleDataArr[i][0], ifBuCmpleDataArr[i][1]), buCompareConst(BYTE_PLACEHOLDER_1, ifBuCmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifBuCmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(buCompareConst, 
            _numberOfBinaryArgs, TR::ifbucmple, resolvedMethodName, _argTypesBinaryByte, TR::Int32, rc, 2, 2, &(ifBuCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(buCompareConst, compareLE(ifBuCmpleDataArr[i][0], ifBuCmpleDataArr[i][1]), buCompareConst(ifBuCmpleDataArr[i][0], BYTE_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifSuCmpgtDataArr) / sizeof(ifSuCmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifSuCmpgt, compareGT(ifSuCmpgtDataArr[i][0], ifSuCmpgtDataArr[i][1]), _ifSuCmpgt(ifSuCmpgtDataArr[i][0], ifSuCmpgtDataArr[i][1])) << ifSuCmpgtDataArr[i][0] << " : " << ifSuCmpgtDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifSuCmpgtDataArr[i][0]), 2, &(ifSuCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGT(ifSuCmpgtDataArr[i][0], ifSuCmpgtDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << ifSuCmpgtDataArr[i][0] << " : " << ifSuCmpgtDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifSuCmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGT(ifSuCmpgtDataArr[i][0], ifSuCmpgtDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, ifSuCmpgtDataArr[i][1])) << ifSuCmpgtDataArr[i][0] << " : " << ifSuCmpgtDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmpgt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifSuCmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGT(ifSuCmpgtDataArr[i][0], ifSuCmpgtDataArr[i][1]), suCompareConst(ifSuCmpgtDataArr[i][0], SHORT_PLACEHOLDER_2)) << ifSuCmpgtDataArr[i][0] << " : " << ifSuCmpgtDataArr[i][1];
      }

   testCaseNum = sizeof(ifSuCmpltDataArr) / sizeof(ifSuCmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifSuCmplt, compareLT(ifSuCmpltDataArr[i][0], ifSuCmpltDataArr[i][1]), _ifSuCmplt(ifSuCmpltDataArr[i][0], ifSuCmpltDataArr[i][1])) << ifSuCmpltDataArr[i][0] << " : " << ifSuCmpltDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifSuCmpltDataArr[i][0]), 2, &(ifSuCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLT(ifSuCmpltDataArr[i][0], ifSuCmpltDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << ifSuCmpltDataArr[i][0] << " : " << ifSuCmpltDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifSuCmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLT(ifSuCmpltDataArr[i][0], ifSuCmpltDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, ifSuCmpltDataArr[i][1])) << ifSuCmpltDataArr[i][0] << " : " << ifSuCmpltDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmplt, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifSuCmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLT(ifSuCmpltDataArr[i][0], ifSuCmpltDataArr[i][1]), suCompareConst(ifSuCmpltDataArr[i][0], SHORT_PLACEHOLDER_2)) << ifSuCmpltDataArr[i][0] << " : " << ifSuCmpltDataArr[i][1];
      }

   testCaseNum = sizeof(ifSuCmpgeDataArr) / sizeof(ifSuCmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifSuCmpge, compareGE(ifSuCmpgeDataArr[i][0], ifSuCmpgeDataArr[i][1]), _ifSuCmpge(ifSuCmpgeDataArr[i][0], ifSuCmpgeDataArr[i][1])) << ifSuCmpgeDataArr[i][0] << " : " << ifSuCmpgeDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifSuCmpgeDataArr[i][0]), 2, &(ifSuCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGE(ifSuCmpgeDataArr[i][0], ifSuCmpgeDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << ifSuCmpgeDataArr[i][0] << " : " << ifSuCmpgeDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifSuCmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGE(ifSuCmpgeDataArr[i][0], ifSuCmpgeDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, ifSuCmpgeDataArr[i][1])) << ifSuCmpgeDataArr[i][0] << " : " << ifSuCmpgeDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmpge, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifSuCmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareGE(ifSuCmpgeDataArr[i][0], ifSuCmpgeDataArr[i][1]), suCompareConst(ifSuCmpgeDataArr[i][0], SHORT_PLACEHOLDER_2)) << ifSuCmpgeDataArr[i][0] << " : " << ifSuCmpgeDataArr[i][1];
      }

   testCaseNum = sizeof(ifSuCmpleDataArr) / sizeof(ifSuCmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifSuCmple, compareLE(ifSuCmpleDataArr[i][0], ifSuCmpleDataArr[i][1]), _ifSuCmple(ifSuCmpleDataArr[i][0], ifSuCmpleDataArr[i][1])) << ifSuCmpleDataArr[i][0] << " : " << ifSuCmpleDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 4, 1, &(ifSuCmpleDataArr[i][0]), 2, &(ifSuCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLE(ifSuCmpleDataArr[i][0], ifSuCmpleDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2)) << ifSuCmpleDataArr[i][0] << " : " << ifSuCmpleDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 1, &(ifSuCmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLE(ifSuCmpleDataArr[i][0], ifSuCmpleDataArr[i][1]), suCompareConst(SHORT_PLACEHOLDER_1, ifSuCmpleDataArr[i][1])) << ifSuCmpleDataArr[i][0] << " : " << ifSuCmpleDataArr[i][1];

      sprintf(resolvedMethodName, "ifSuCmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(suCompareConst, 
            _numberOfBinaryArgs, TR::ifsucmple, resolvedMethodName, _argTypesBinaryShort, TR::Int32, rc, 2, 2, &(ifSuCmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(suCompareConst, compareLE(ifSuCmpleDataArr[i][0], ifSuCmpleDataArr[i][1]), suCompareConst(ifSuCmpleDataArr[i][0], SHORT_PLACEHOLDER_2)) << ifSuCmpleDataArr[i][0] << " : " << ifSuCmpleDataArr[i][1];
      }
   }

void
PPCOpCodesTest::invokeAddressTests()
   {
   int32_t rc = 0;

   int8_t byteDataArr[] = {BYTE_NEG, BYTE_POS, BYTE_MAXIMUM, BYTE_MINIMUM, BYTE_ZERO};
   int16_t shortDataArr[] = {SHORT_NEG, SHORT_POS, SHORT_MAXIMUM, SHORT_MINIMUM, SHORT_ZERO};
   int32_t intDataArr[] = {INT_NEG, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO};
   uint8_t ubyteDataArr[] = {UBYTE_POS, UBYTE_MAXIMUM, UBYTE_MINIMUM};
   uint64_t ulongDataArr[] = {ULONG_POS, ULONG_MAXIMUM, ULONG_MINIMUM};
   uintptrj_t aUnaryDataArr[] =
      {
      (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_ZERO,
      (uintptrj_t) &LONG_POS,
      (uintptrj_t) &LONG_MAXIMUM,
      (uintptrj_t) &LONG_ZERO
      };

   uintptrj_t acmpeqDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MINIMUM
      };
   uintptrj_t acmpneDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_POS
      };
   uintptrj_t acmpltDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MINIMUM
      };
   uintptrj_t acmpgeDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MINIMUM
      };
   uintptrj_t acmpleDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MAXIMUM
      };
   uintptrj_t acmpgtDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_POS
      };
   uintptrj_t ifacmpeqDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MINIMUM
      };
   uintptrj_t ifacmpneDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_POS
      };
   uintptrj_t ifacmpltDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MINIMUM
      };
   uintptrj_t ifacmpgeDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MINIMUM
      };
   uintptrj_t ifacmpleDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_MAXIMUM
      };
   uintptrj_t ifacmpgtDataArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM,  (uintptrj_t) &INT_MINIMUM,
      (uintptrj_t) &INT_MINIMUM,  (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,   (uintptrj_t) &INT_POS
      };

   int32_t aternaryChild1Arr[] =
      {
      INT_MAXIMUM,
      INT_MAXIMUM,
      INT_MAXIMUM,
      INT_MINIMUM,
      INT_MINIMUM,
      INT_MINIMUM,
      INT_NEG,
      INT_NEG,
      INT_NEG,
      INT_POS,
      INT_POS,
      INT_POS,
      INT_ZERO,
      INT_ZERO,
      INT_ZERO
      };
   uintptrj_t aternaryArr[][2] =
      {
      (uintptrj_t) &INT_MAXIMUM, (uintptrj_t) &INT_ZERO,
      (uintptrj_t) &INT_POS,     (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_ZERO,    (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MAXIMUM, (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_POS,     (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_ZERO,    (uintptrj_t) &INT_ZERO,
      (uintptrj_t) &INT_MAXIMUM, (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_POS,     (uintptrj_t) &INT_ZERO,
      (uintptrj_t) &INT_ZERO,    (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_MAXIMUM, (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_POS,     (uintptrj_t) &INT_MAXIMUM,
      (uintptrj_t) &INT_ZERO,    (uintptrj_t) &INT_ZERO,
      (uintptrj_t) &INT_MAXIMUM, (uintptrj_t) &INT_ZERO,
      (uintptrj_t) &INT_POS,     (uintptrj_t) &INT_POS,
      (uintptrj_t) &INT_ZERO,    (uintptrj_t) &INT_MAXIMUM
      };

   uint32_t testCaseNum = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];
   signatureCharB_L_testMethodType *b2aConst = 0;
   signatureCharS_L_testMethodType *s2aConst = 0;
   signatureCharI_L_testMethodType *i2aConst = 0;
   signatureCharL_J_testMethodType *a2lConst = 0;
   signatureCharL_S_testMethodType *a2sConst = 0;
   signatureCharL_B_testMethodType *a2bConst = 0;
   unsignedSignatureCharB_L_testMethodType *bu2aConst = 0;
   unsignedSignatureCharJ_L_testMethodType *lu2aConst = 0;

   signatureCharLL_I_testMethodType *aCompareConst = 0;
   signatureCharILL_L_testMethodType *aTernaryConst = 0;



   //address convert unary opcodes
   testCaseNum = sizeof(byteDataArr) / sizeof(byteDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_b2a, convert(byteDataArr[i], ADDRESS_PLACEHOLDER_1), _b2a(byteDataArr[i]));

      sprintf(resolvedMethodName, "b2aConst%d", i + 1);
      compileOpCodeMethod(b2aConst, 
            _numberOfUnaryArgs, TR::b2a, resolvedMethodName, _argTypesUnaryByte, TR::Address, rc, 2, 1, &byteDataArr[i]);
      OMR_CT_EXPECT_EQ(b2aConst, convert(byteDataArr[i], ADDRESS_PLACEHOLDER_1), b2aConst(BYTE_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(shortDataArr) / sizeof(shortDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_s2a, convert(shortDataArr[i], ADDRESS_PLACEHOLDER_1), _s2a(shortDataArr[i]));

      sprintf(resolvedMethodName, "s2aConst%d", i + 1);
      compileOpCodeMethod(s2aConst, 
            _numberOfUnaryArgs, TR::s2a, resolvedMethodName, _argTypesUnaryShort, TR::Address, rc, 2, 1, &shortDataArr[i]);
      OMR_CT_EXPECT_EQ(s2aConst, convert(shortDataArr[i], ADDRESS_PLACEHOLDER_1), s2aConst(SHORT_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(intDataArr) / sizeof(intDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_i2a, convert(intDataArr[i], ADDRESS_PLACEHOLDER_1), _i2a(intDataArr[i]));

      sprintf(resolvedMethodName, "i2aConst%d", i + 1);
      compileOpCodeMethod(i2aConst, 
            _numberOfUnaryArgs, TR::i2a, resolvedMethodName, _argTypesUnaryInt, TR::Address, rc, 2, 1, &intDataArr[i]);
      OMR_CT_EXPECT_EQ(i2aConst, convert(intDataArr[i], ADDRESS_PLACEHOLDER_1), i2aConst(INT_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(ubyteDataArr) / sizeof(ubyteDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_bu2a, convert(ubyteDataArr[i], ADDRESS_PLACEHOLDER_1), _bu2a(ubyteDataArr[i]));

      sprintf(resolvedMethodName, "bu2aConst%d", i + 1);
      compileOpCodeMethod(bu2aConst, 
            _numberOfUnaryArgs, TR::bu2a, resolvedMethodName, _argTypesUnaryByte, TR::Address, rc, 2, 1, &ubyteDataArr[i]);
      OMR_CT_EXPECT_EQ(bu2aConst, convert(ubyteDataArr[i], ADDRESS_PLACEHOLDER_1), bu2aConst(BYTE_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(ulongDataArr) / sizeof(ulongDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_lu2a, convert(ulongDataArr[i], ADDRESS_PLACEHOLDER_1), _lu2a(ulongDataArr[i]));

      sprintf(resolvedMethodName, "lu2aConst%d", i + 1);
      compileOpCodeMethod(lu2aConst, 
            _numberOfUnaryArgs, TR::lu2a, resolvedMethodName, _argTypesUnaryLong, TR::Address, rc, 2, 1, &ulongDataArr[i]);
      OMR_CT_EXPECT_EQ(lu2aConst, convert(ulongDataArr[i], ADDRESS_PLACEHOLDER_1), lu2aConst(LONG_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(aUnaryDataArr) / sizeof(aUnaryDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_a2l, convert(aUnaryDataArr[i], LONG_POS), _a2l(aUnaryDataArr[i]));

      sprintf(resolvedMethodName, "a2lConst%d", i + 1);
      compileOpCodeMethod(a2lConst, _numberOfUnaryArgs, TR::a2l, resolvedMethodName, _argTypesUnaryAddress, TR::Int64, rc, 2, 1, &aUnaryDataArr[i]);
      OMR_CT_EXPECT_EQ(a2lConst, convert(aUnaryDataArr[i], LONG_POS), a2lConst(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(aUnaryDataArr) / sizeof(aUnaryDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_a2s, convert(aUnaryDataArr[i], SHORT_POS), _a2s(aUnaryDataArr[i]));

      sprintf(resolvedMethodName, "a2sConst%d", i + 1);
      compileOpCodeMethod(a2sConst, 
            _numberOfUnaryArgs, TR::a2s, resolvedMethodName, _argTypesUnaryAddress, TR::Int16, rc, 2, 1, &aUnaryDataArr[i]);
      OMR_CT_EXPECT_EQ(a2sConst, convert(aUnaryDataArr[i], SHORT_POS), a2sConst(ADDRESS_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(aUnaryDataArr) / sizeof(aUnaryDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_a2b, convert(aUnaryDataArr[i], BYTE_POS), _a2b(aUnaryDataArr[i]));

      sprintf(resolvedMethodName, "a2bConst%d", i + 1);
      compileOpCodeMethod(a2bConst, 
            _numberOfUnaryArgs, TR::a2b, resolvedMethodName, _argTypesUnaryAddress, TR::Int8, rc, 2, 1, &aUnaryDataArr[i]);
      OMR_CT_EXPECT_EQ(a2bConst, convert(aUnaryDataArr[i], BYTE_POS), a2bConst(ADDRESS_PLACEHOLDER_1));
      }

   //address compare
   testCaseNum = sizeof(acmpeqDataArr) / sizeof(acmpeqDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_acmpeq, compareEQ(acmpeqDataArr[i][0], acmpeqDataArr[i][1]), _acmpeq(acmpeqDataArr[i][0], acmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "aCmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpeq, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(acmpeqDataArr[i][0]), 2, &(acmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareEQ(acmpeqDataArr[i][0], acmpeqDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "aCmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpeq, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(acmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareEQ(acmpeqDataArr[i][0], acmpeqDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, acmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "aCmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpeq, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(acmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareEQ(acmpeqDataArr[i][0], acmpeqDataArr[i][1]), aCompareConst(acmpeqDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(acmpneDataArr) / sizeof(acmpneDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_acmpne, compareNE(acmpneDataArr[i][0], acmpneDataArr[i][1]), _acmpne(acmpneDataArr[i][0], acmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "aCmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpne, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(acmpneDataArr[i][0]), 2, &(acmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareNE(acmpneDataArr[i][0], acmpneDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "aCmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpne, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(acmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareNE(acmpneDataArr[i][0], acmpneDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, acmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "aCmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpne, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(acmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareNE(acmpneDataArr[i][0], acmpneDataArr[i][1]), aCompareConst(acmpneDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(acmpgtDataArr) / sizeof(acmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_acmpgt, compareGT(acmpgtDataArr[i][0], acmpgtDataArr[i][1]), _acmpgt(acmpgtDataArr[i][0], acmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpgt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(acmpgtDataArr[i][0]), 2, &(acmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGT(acmpgtDataArr[i][0], acmpgtDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "acmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpgt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(acmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGT(acmpgtDataArr[i][0], acmpgtDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, acmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpgt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(acmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGT(acmpgtDataArr[i][0], acmpgtDataArr[i][1]), aCompareConst(acmpgtDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(acmpltDataArr) / sizeof(acmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_acmplt, compareLT(acmpltDataArr[i][0], acmpltDataArr[i][1]), _acmplt(acmpltDataArr[i][0], acmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmplt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(acmpltDataArr[i][0]), 2, &(acmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLT(acmpltDataArr[i][0], acmpltDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "acmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmplt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(acmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLT(acmpltDataArr[i][0], acmpltDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, acmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmplt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(acmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLT(acmpltDataArr[i][0], acmpltDataArr[i][1]), aCompareConst(acmpltDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(acmpgeDataArr) / sizeof(acmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_acmpge, compareGE(acmpgeDataArr[i][0], acmpgeDataArr[i][1]), _acmpge(acmpgeDataArr[i][0], acmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpge, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(acmpgeDataArr[i][0]), 2, &(acmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGE(acmpgeDataArr[i][0], acmpgeDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "acmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpge, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(acmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGE(acmpgeDataArr[i][0], acmpgeDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, acmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmpge, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(acmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGE(acmpgeDataArr[i][0], acmpgeDataArr[i][1]), aCompareConst(acmpgeDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(acmpleDataArr) / sizeof(acmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_acmple, compareLE(acmpleDataArr[i][0], acmpleDataArr[i][1]), _acmple(acmpleDataArr[i][0], acmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmple, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(acmpleDataArr[i][0]), 2, &(acmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLE(acmpleDataArr[i][0], acmpleDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "acmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmple, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(acmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLE(acmpleDataArr[i][0], acmpleDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, acmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "acmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::acmple, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(acmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLE(acmpleDataArr[i][0], acmpleDataArr[i][1]), aCompareConst(acmpleDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   //address ifcompare
   testCaseNum = sizeof(ifacmpeqDataArr) / sizeof(ifacmpeqDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_ifacmpeq, compareEQ(ifacmpeqDataArr[i][0], ifacmpeqDataArr[i][1]), _ifacmpeq(ifacmpeqDataArr[i][0], ifacmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpeqConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpeq, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(ifacmpeqDataArr[i][0]), 2, &(ifacmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareEQ(ifacmpeqDataArr[i][0], ifacmpeqDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifacmpeqConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpeq, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(ifacmpeqDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareEQ(ifacmpeqDataArr[i][0], ifacmpeqDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ifacmpeqDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpeqConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpeq, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(ifacmpeqDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareEQ(ifacmpeqDataArr[i][0], ifacmpeqDataArr[i][1]), aCompareConst(ifacmpeqDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifacmpneDataArr) / sizeof(ifacmpneDataArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_ifacmpne, compareNE(ifacmpneDataArr[i][0], ifacmpneDataArr[i][1]), _ifacmpne(ifacmpneDataArr[i][0], ifacmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpneConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpne, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(ifacmpneDataArr[i][0]), 2, &(ifacmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareNE(ifacmpneDataArr[i][0], ifacmpneDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifacmpneConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpne, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(ifacmpneDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareNE(ifacmpneDataArr[i][0], ifacmpneDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ifacmpneDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpneConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpne, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(ifacmpneDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareNE(ifacmpneDataArr[i][0], ifacmpneDataArr[i][1]), aCompareConst(ifacmpneDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifacmpgtDataArr) / sizeof(ifacmpgtDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifacmpgt, compareGT(ifacmpgtDataArr[i][0], ifacmpgtDataArr[i][1]), _ifacmpgt(ifacmpgtDataArr[i][0], ifacmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpgtConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpgt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(ifacmpgtDataArr[i][0]), 2, &(ifacmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGT(ifacmpgtDataArr[i][0], ifacmpgtDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifacmpgtConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpgt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(ifacmpgtDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGT(ifacmpgtDataArr[i][0], ifacmpgtDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ifacmpgtDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpgtConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpgt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(ifacmpgtDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGT(ifacmpgtDataArr[i][0], ifacmpgtDataArr[i][1]), aCompareConst(ifacmpgtDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifacmpltDataArr) / sizeof(ifacmpltDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifacmplt, compareLT(ifacmpltDataArr[i][0], ifacmpltDataArr[i][1]), _ifacmplt(ifacmpltDataArr[i][0], ifacmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpltConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmplt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(ifacmpltDataArr[i][0]), 2, &(ifacmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLT(ifacmpltDataArr[i][0], ifacmpltDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifacmpltConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmplt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(ifacmpltDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLT(ifacmpltDataArr[i][0], ifacmpltDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ifacmpltDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpltConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmplt, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(ifacmpltDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLT(ifacmpltDataArr[i][0], ifacmpltDataArr[i][1]), aCompareConst(ifacmpltDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifacmpgeDataArr) / sizeof(ifacmpgeDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifacmpge, compareGE(ifacmpgeDataArr[i][0], ifacmpgeDataArr[i][1]), _ifacmpge(ifacmpgeDataArr[i][0], ifacmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpgeConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpge, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(ifacmpgeDataArr[i][0]), 2, &(ifacmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGE(ifacmpgeDataArr[i][0], ifacmpgeDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifacmpgeConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpge, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(ifacmpgeDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGE(ifacmpgeDataArr[i][0], ifacmpgeDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ifacmpgeDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpgeConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmpge, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(ifacmpgeDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareGE(ifacmpgeDataArr[i][0], ifacmpgeDataArr[i][1]), aCompareConst(ifacmpgeDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   testCaseNum = sizeof(ifacmpleDataArr) / sizeof(ifacmpleDataArr[0]);
   for(uint32_t i = 0; i < testCaseNum; ++i)
      {
      OMR_CT_EXPECT_EQ(_ifacmple, compareLE(ifacmpleDataArr[i][0], ifacmpleDataArr[i][1]), _ifacmple(ifacmpleDataArr[i][0], ifacmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpleConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmple, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 4, 1, &(ifacmpleDataArr[i][0]), 2, &(ifacmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLE(ifacmpleDataArr[i][0], ifacmpleDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "ifacmpleConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmple, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 1, &(ifacmpleDataArr[i][0]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLE(ifacmpleDataArr[i][0], ifacmpleDataArr[i][1]), aCompareConst(ADDRESS_PLACEHOLDER_1, ifacmpleDataArr[i][1]));

      sprintf(resolvedMethodName, "ifacmpleConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aCompareConst, 
            _numberOfBinaryArgs, TR::ifacmple, resolvedMethodName, _argTypesBinaryAddress, TR::Int32, rc, 2, 2, &(ifacmpleDataArr[i][1]));
      OMR_CT_EXPECT_EQ(aCompareConst, compareLE(ifacmpleDataArr[i][0], ifacmpleDataArr[i][1]), aCompareConst(ifacmpleDataArr[i][0], ADDRESS_PLACEHOLDER_2));
      }

   TR_ASSERT((sizeof(aternaryChild1Arr) / sizeof(aternaryChild1Arr[0])) == sizeof(aternaryArr) / sizeof(aternaryArr[0]),
         "Child1 array size is not equal to Child2 and Child3 array");
   testCaseNum = sizeof(aternaryChild1Arr) / sizeof(aternaryChild1Arr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_aternary, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), _aternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]));

      sprintf(resolvedMethodName, "aTernaryConst1_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 6, 1, &aternaryChild1Arr[i], 2, &aternaryArr[i][0], 3, &aternaryArr[i][1]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(INT_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2, ADDRESS_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "aTernaryConst2_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 4, 1, &aternaryChild1Arr[i], 2, &aternaryArr[i][0]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(INT_PLACEHOLDER_1, ADDRESS_PLACEHOLDER_2, aternaryArr[i][1]));

      sprintf(resolvedMethodName, "aTernaryConst3_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 4, 1, &aternaryChild1Arr[i], 3, &aternaryArr[i][1]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(INT_PLACEHOLDER_1, aternaryArr[i][0], ADDRESS_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "aTernaryConst4_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 4, 2, &aternaryArr[i][0], 3, &aternaryArr[i][1]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(aternaryChild1Arr[i], ADDRESS_PLACEHOLDER_2, ADDRESS_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "aTernaryConst5_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 2, 1, &aternaryChild1Arr[i]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(INT_PLACEHOLDER_1, aternaryArr[i][0], aternaryArr[i][1]));

      sprintf(resolvedMethodName, "aTernaryConst6_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 2, 2, &aternaryArr[i][0]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(aternaryChild1Arr[i], ADDRESS_PLACEHOLDER_1, aternaryArr[i][1]));

      sprintf(resolvedMethodName, "aTernaryConst7_TestCase%d", i + 1);
      compileOpCodeMethod(aTernaryConst, 
            _numberOfTernaryArgs, TR::aternary, resolvedMethodName, _argTypesTernaryAddress, TR::Address, rc, 2, 3, &aternaryArr[i][1]);
      OMR_CT_EXPECT_EQ(aTernaryConst, ternary(aternaryChild1Arr[i], aternaryArr[i][0], aternaryArr[i][1]), aTernaryConst(aternaryChild1Arr[i], aternaryArr[i][0], ADDRESS_PLACEHOLDER_1));
      }

   }

void
PPCOpCodesTest::invokeTernaryTests()
   {
   int32_t rc = 0;
   int32_t bternaryChild1Arr[] =
      {
      INT_MAXIMUM, INT_POS, INT_MAXIMUM, INT_MINIMUM, INT_ZERO, INT_ZERO, INT_MINIMUM, INT_POS, INT_NEG,
      INT_NEG, INT_NEG, INT_ZERO, INT_POS, INT_MAXIMUM, INT_NEG, INT_ZERO, INT_MINIMUM, INT_POS
      };
   int32_t sternaryChild1Arr[] =
      {
      INT_MAXIMUM, INT_MINIMUM, INT_POS, INT_ZERO, INT_NEG, INT_MINIMUM, INT_POS, INT_MAXIMUM, INT_ZERO,
      INT_NEG, INT_NEG, INT_ZERO, INT_POS, INT_NEG, INT_ZERO, INT_MINIMUM, INT_MAXIMUM, INT_POS
      };

   int8_t byteDataArr[][2] =
      {
      BYTE_NEG, BYTE_MINIMUM,
      BYTE_NEG, BYTE_POS,
      BYTE_NEG, BYTE_POS,
      BYTE_MAXIMUM, BYTE_ZERO,
      BYTE_ZERO, BYTE_POS,
      BYTE_ZERO, BYTE_POS,
      BYTE_ZERO, BYTE_MAXIMUM,
      BYTE_POS, BYTE_NEG,
      BYTE_POS, BYTE_MINIMUM,
      BYTE_MAXIMUM, BYTE_POS,
      BYTE_MINIMUM, BYTE_ZERO,
      BYTE_MINIMUM, BYTE_MAXIMUM,
      BYTE_NEG, BYTE_MAXIMUM,
      BYTE_ZERO, BYTE_NEG,
      BYTE_POS, BYTE_ZERO,
      BYTE_MAXIMUM, BYTE_NEG,
      BYTE_ZERO, BYTE_MINIMUM,
      BYTE_MINIMUM, BYTE_POS
      };

   int16_t shortDataArr[][2] =
      {
      SHORT_NEG, SHORT_MINIMUM,
      SHORT_NEG, SHORT_POS,
      SHORT_NEG, SHORT_POS,
      SHORT_MAXIMUM, SHORT_ZERO,
      SHORT_ZERO, SHORT_POS,
      SHORT_ZERO, SHORT_POS,
      SHORT_ZERO, SHORT_MAXIMUM,
      SHORT_POS, SHORT_NEG,
      SHORT_POS, SHORT_MINIMUM,
      SHORT_MAXIMUM, SHORT_POS,
      SHORT_MINIMUM, SHORT_ZERO,
      SHORT_MINIMUM, SHORT_MAXIMUM,
      SHORT_NEG, SHORT_MAXIMUM,
      SHORT_ZERO, SHORT_NEG,
      SHORT_POS, SHORT_ZERO,
      SHORT_MAXIMUM, SHORT_NEG,
      SHORT_ZERO, SHORT_MINIMUM,
      SHORT_MINIMUM, SHORT_POS
      };

   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];
   uint32_t testCaseNumCheck = 0;
   uint32_t testCaseNum = 0;

   signatureCharIBB_B_testMethodType * bTernaryConst = 0;
   signatureCharISS_S_testMethodType * sTernaryConst = 0;

   testCaseNum = sizeof(bternaryChild1Arr) / sizeof(bternaryChild1Arr[0]);
   testCaseNumCheck = sizeof(byteDataArr) / sizeof(byteDataArr[0]);
   TR_ASSERT( (testCaseNum > 0) && (testCaseNum == testCaseNumCheck), "There is problem in bternary input array");
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_bternary, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), _bternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]));

      sprintf(resolvedMethodName, "bTernaryConst1_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 6, 1, &bternaryChild1Arr[i], 2, &byteDataArr[i][0], 3, &byteDataArr[i][1]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2, BYTE_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "bTernaryConst2_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 4, 1, &bternaryChild1Arr[i], 2, &byteDataArr[i][0]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2, byteDataArr[i][1]));

      sprintf(resolvedMethodName, "bTernaryConst3_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 4, 1, &bternaryChild1Arr[i], 3, &byteDataArr[i][1]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(BYTE_PLACEHOLDER_1, byteDataArr[i][0], BYTE_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "bTernaryConst4_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 4, 2, &byteDataArr[i][0], 3, &byteDataArr[i][1]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(bternaryChild1Arr[i], BYTE_PLACEHOLDER_2, BYTE_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "bTernaryConst5_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 2, 1, &bternaryChild1Arr[i]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(BYTE_PLACEHOLDER_1, byteDataArr[i][0], byteDataArr[i][1]));

      sprintf(resolvedMethodName, "bTernaryConst6_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 2, 2, &byteDataArr[i][0]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(bternaryChild1Arr[i], BYTE_PLACEHOLDER_1, byteDataArr[i][1]));

      sprintf(resolvedMethodName, "bTernaryConst7_Testcase%d", i + 1);
      compileOpCodeMethod(bTernaryConst, _numberOfTernaryArgs, TR::bternary,
            resolvedMethodName, _argTypesTernaryByte, TR::Int8, rc, 2, 3, &byteDataArr[i][1]);
      OMR_CT_EXPECT_EQ(bTernaryConst, ternary(bternaryChild1Arr[i], byteDataArr[i][0], byteDataArr[i][1]), bTernaryConst(bternaryChild1Arr[i], byteDataArr[i][0], BYTE_PLACEHOLDER_1));
      }

   testCaseNum = sizeof(sternaryChild1Arr) / sizeof(sternaryChild1Arr[0]);
   testCaseNumCheck = sizeof(shortDataArr) / sizeof(shortDataArr[0]);
   TR_ASSERT( (testCaseNum > 0) && (testCaseNum == testCaseNumCheck), "There is problem in sternary input array");
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_sternary, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), _sternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]));

      sprintf(resolvedMethodName, "sTernaryConst1_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 6, 1, &sternaryChild1Arr[i], 2, &shortDataArr[i][0], 3, &shortDataArr[i][1]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2, SHORT_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "sTernaryConst2_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 4, 1, &sternaryChild1Arr[i], 2, &shortDataArr[i][0]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2, shortDataArr[i][1]));

      sprintf(resolvedMethodName, "sTernaryConst3_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 4, 1, &sternaryChild1Arr[i], 3, &shortDataArr[i][1]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(SHORT_PLACEHOLDER_1, shortDataArr[i][0], SHORT_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "sTernaryConst4_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 4, 2, &shortDataArr[i][0], 3, &shortDataArr[i][1]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(sternaryChild1Arr[i], SHORT_PLACEHOLDER_2, SHORT_PLACEHOLDER_3));

      sprintf(resolvedMethodName, "sTernaryConst5_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 2, 1, &sternaryChild1Arr[i]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(SHORT_PLACEHOLDER_1, shortDataArr[i][0], shortDataArr[i][1]));

      sprintf(resolvedMethodName, "sTernaryConst6_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 2, 2, &shortDataArr[i][0]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(sternaryChild1Arr[i], SHORT_PLACEHOLDER_1, shortDataArr[i][1]));

      sprintf(resolvedMethodName, "sTernaryConst7_Testcase%d", i + 1);
      compileOpCodeMethod(sTernaryConst, _numberOfTernaryArgs, TR::sternary,
            resolvedMethodName, _argTypesTernaryShort, TR::Int16, rc, 2, 3, &shortDataArr[i][1]);
      OMR_CT_EXPECT_EQ(sTernaryConst, ternary(sternaryChild1Arr[i], shortDataArr[i][0], shortDataArr[i][1]), sTernaryConst(sternaryChild1Arr[i], shortDataArr[i][0], SHORT_PLACEHOLDER_1));
      }
   }

void
PPCOpCodesTest::invokeBitwiseTests()
   {
   int32_t rc = 0;
   char resolvedMethodName [RESOLVED_METHOD_NAME_LENGTH];
   uint32_t testCaseNum = 0;

   int8_t byteAndArr [][2] =
      {
      BYTE_MINIMUM, BYTE_MAXIMUM,
      BYTE_NEG, BYTE_NEG,
      BYTE_MAXIMUM, BYTE_ZERO,
      BYTE_MAXIMUM, BYTE_NEG,
      BYTE_MINIMUM, BYTE_ZERO,
      BYTE_NEG, BYTE_MAXIMUM,
      BYTE_POS, BYTE_MINIMUM
      };
   int8_t byteOrArr [][2] =
      {
      BYTE_MAXIMUM, BYTE_POS,
      BYTE_ZERO, BYTE_ZERO
      };
   int8_t byteXorArr [][2] =
      {
      BYTE_ZERO, BYTE_NEG
      };
   int16_t shortAndArr [][2] =
      {
      SHORT_MAXIMUM, SHORT_MINIMUM,
      SHORT_ZERO, SHORT_POS
      };
   int16_t shortOrArr [][2] =
      {
      SHORT_ZERO, SHORT_MINIMUM,
      SHORT_POS, SHORT_NEG,
      SHORT_ZERO, SHORT_MAXIMUM,
      SHORT_MINIMUM, SHORT_NEG,
      SHORT_NEG, SHORT_MINIMUM,
      SHORT_POS, SHORT_ZERO
      };
   int16_t shortXorArr [][2] =
      {
      SHORT_POS, SHORT_MAXIMUM,
      SHORT_MINIMUM, SHORT_POS,
      SHORT_NEG, SHORT_POS,
      SHORT_NEG, SHORT_NEG,
      SHORT_MINIMUM, SHORT_MINIMUM,
      SHORT_POS, SHORT_POS,
      SHORT_MAXIMUM, SHORT_MAXIMUM
      };

   signatureCharSS_S_testMethodType * sBitwiseConst = 0;
   signatureCharBB_B_testMethodType * bBitwiseConst = 0;

   //band
   testCaseNum = sizeof(byteAndArr) / sizeof(byteAndArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_bAnd, tand(byteAndArr[i][0], byteAndArr[i][1]), _bAnd(byteAndArr[i][0], byteAndArr[i][1]));

      sprintf(resolvedMethodName, "bAndConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::band, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 4, 1, &(byteAndArr[i][0]), 2, &(byteAndArr[i][1]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, tand(byteAndArr[i][0], byteAndArr[i][1]), bBitwiseConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "bAndConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::band, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 2, 1, &(byteAndArr[i][0]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, tand(byteAndArr[i][0], byteAndArr[i][1]), bBitwiseConst(BYTE_PLACEHOLDER_1, byteAndArr[i][1]));

      sprintf(resolvedMethodName, "bAndConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::band, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 2, 2, &(byteAndArr[i][1]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, tand(byteAndArr[i][0], byteAndArr[i][1]), bBitwiseConst(byteAndArr[i][0], BYTE_PLACEHOLDER_2));
     }

   //bor
   testCaseNum = sizeof(byteOrArr) / sizeof(byteOrArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_bOr, tor(byteOrArr[i][0], byteOrArr[i][1]), _bOr(byteOrArr[i][0], byteOrArr[i][1]));

      sprintf(resolvedMethodName, "bOrConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::bor, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 4, 1, &(byteOrArr[i][0]), 2, &(byteOrArr[i][1]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, tor(byteOrArr[i][0], byteOrArr[i][1]), bBitwiseConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "bOrConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::bor, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 2, 1, &(byteOrArr[i][0]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, tor(byteOrArr[i][0], byteOrArr[i][1]), bBitwiseConst(BYTE_PLACEHOLDER_1, byteOrArr[i][1]));

      sprintf(resolvedMethodName, "bOrConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::bor, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 2, 2, &(byteOrArr[i][1]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, tor(byteOrArr[i][0], byteOrArr[i][1]), bBitwiseConst(byteOrArr[i][0], BYTE_PLACEHOLDER_2));
     }

   //bxor
   testCaseNum = sizeof(byteXorArr) / sizeof(byteXorArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_bXor, txor(byteXorArr[i][0], byteXorArr[i][1]), _bXor(byteXorArr[i][0], byteXorArr[i][1]));

      sprintf(resolvedMethodName, "bXorConst1_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::bxor, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 4, 1, &(byteXorArr[i][0]), 2, &(byteXorArr[i][1]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, txor(byteXorArr[i][0], byteXorArr[i][1]), bBitwiseConst(BYTE_PLACEHOLDER_1, BYTE_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "bXorConst2_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::bxor, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 2, 1, &(byteXorArr[i][0]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, txor(byteXorArr[i][0], byteXorArr[i][1]), bBitwiseConst(BYTE_PLACEHOLDER_1, byteXorArr[i][1]));

      sprintf(resolvedMethodName, "bXorConst3_TestCase%d", i + 1);
      compileOpCodeMethod(bBitwiseConst, 
            _numberOfBinaryArgs, TR::bxor, resolvedMethodName, _argTypesBinaryByte, TR::Int8, rc, 2, 2, &(byteXorArr[i][1]));
      OMR_CT_EXPECT_EQ(bBitwiseConst, txor(byteXorArr[i][0], byteXorArr[i][1]), bBitwiseConst(byteXorArr[i][0], BYTE_PLACEHOLDER_2));
      }

   //sand
   testCaseNum = sizeof(shortAndArr) / sizeof(shortAndArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_sAnd, tand(shortAndArr[i][0], shortAndArr[i][1]), _sAnd(shortAndArr[i][0], shortAndArr[i][1]));

      sprintf(resolvedMethodName, "sAndConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sand, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 4, 1, &(shortAndArr[i][0]), 2, &(shortAndArr[i][1]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, tand(shortAndArr[i][0], shortAndArr[i][1]), sBitwiseConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sAndConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sand, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 2, 1, &(shortAndArr[i][0]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, tand(shortAndArr[i][0], shortAndArr[i][1]), sBitwiseConst(SHORT_PLACEHOLDER_1, shortAndArr[i][1]));

      sprintf(resolvedMethodName, "sAndConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sand, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 2, 2, &(shortAndArr[i][1]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, tand(shortAndArr[i][0], shortAndArr[i][1]), sBitwiseConst(shortAndArr[i][0], SHORT_PLACEHOLDER_2));
      }

   //sor
   testCaseNum = sizeof(shortOrArr) / sizeof(shortOrArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_sOr, tor(shortOrArr[i][0], shortOrArr[i][1]), _sOr(shortOrArr[i][0], shortOrArr[i][1]));

      sprintf(resolvedMethodName, "sOrConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sor, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 4, 1, &(shortOrArr[i][0]), 2, &(shortOrArr[i][1]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, tor(shortOrArr[i][0], shortOrArr[i][1]), sBitwiseConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sOrConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sor, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 2, 1, &(shortOrArr[i][0]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, tor(shortOrArr[i][0], shortOrArr[i][1]), sBitwiseConst(SHORT_PLACEHOLDER_1, shortOrArr[i][1]));

      sprintf(resolvedMethodName, "sOrConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sor, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 2, 2, &(shortOrArr[i][1]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, tor(shortOrArr[i][0], shortOrArr[i][1]), sBitwiseConst(shortOrArr[i][0], SHORT_PLACEHOLDER_2));
     }

   //sxor
   testCaseNum = sizeof(shortXorArr) / sizeof(shortXorArr[0]);
   for (int32_t i = 0 ; i < testCaseNum ; i++)
      {
      OMR_CT_EXPECT_EQ(_sXor, txor(shortXorArr[i][0], shortXorArr[i][1]), _sXor(shortXorArr[i][0], shortXorArr[i][1]));

      sprintf(resolvedMethodName, "sXorConst1_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sxor, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 4, 1, &(shortXorArr[i][0]), 2, &(shortXorArr[i][1]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, txor(shortXorArr[i][0], shortXorArr[i][1]), sBitwiseConst(SHORT_PLACEHOLDER_1, SHORT_PLACEHOLDER_2));

      sprintf(resolvedMethodName, "sXorConst2_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sxor, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 2, 1, &(shortXorArr[i][0]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, txor(shortXorArr[i][0], shortXorArr[i][1]), sBitwiseConst(SHORT_PLACEHOLDER_1, shortXorArr[i][1]));

      sprintf(resolvedMethodName, "sXorConst3_TestCase%d", i + 1);
      compileOpCodeMethod(sBitwiseConst, 
            _numberOfBinaryArgs, TR::sxor, resolvedMethodName, _argTypesBinaryShort, TR::Int16, rc, 2, 2, &(shortXorArr[i][1]));
      OMR_CT_EXPECT_EQ(sBitwiseConst, txor(shortXorArr[i][0], shortXorArr[i][1]), sBitwiseConst(shortXorArr[i][0], SHORT_PLACEHOLDER_2));
     }
   }
} // namespace TestCompiler

#if defined(TR_TARGET_POWER)
//groups by testname
TEST(JITPPCOpCodesTest, UnaryTest)
   {
   ::TestCompiler::PPCOpCodesTest PPCUnaryTest;
   PPCUnaryTest.compileUnaryTestMethods();
   PPCUnaryTest.invokeUnaryTests();
   }

TEST(JITPPCOpCodesTest, MemoryOperationTest)
   {
   ::TestCompiler::PPCOpCodesTest PPCMemoryOperationTest;
   PPCMemoryOperationTest.compileMemoryOperationTestMethods();
   PPCMemoryOperationTest.invokeMemoryOperationTests();
   }

TEST(JITPPCOpCodesTest, TernaryTest)
   {
   ::TestCompiler::PPCOpCodesTest PPCTernaryTest;
   PPCTernaryTest.compileTernaryTestMethods();
   PPCTernaryTest.invokeTernaryTests();
   }

TEST(JITPPCOpCodesTest, CompareTest)
   {
   ::TestCompiler::PPCOpCodesTest PPCCompareTest;
   PPCCompareTest.compileCompareTestMethods();
   PPCCompareTest.invokeCompareTests();
   }

TEST(JITPPCOpCodesTest, BitwiseTest)
   {
   ::TestCompiler::PPCOpCodesTest PPCBitwiseTest;
   PPCBitwiseTest.compileBitwiseTestMethods();
   PPCBitwiseTest.invokeBitwiseTests();
   }

TEST(JITPPCOpCodesTest, PPCAddressTest)
   {
   ::TestCompiler::PPCOpCodesTest PPCAddressTest;
   PPCAddressTest.compileAddressTestMethods();
   PPCAddressTest.invokeAddressTests();
   }
#endif
