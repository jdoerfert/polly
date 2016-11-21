; RUN: opt %loadPolly -polly-scops -analyze < %s | FileCheck %s
; RUN: opt %loadPolly -polly-codegen -S < %s

@A = common global [1536 x float] zeroinitializer

; CHECK:      Assumed Context:
; CHECK-NEXT: {  :  }
; CHECK-NEXT: Invalid Context:
; CHECK-NEXT: {  : 1 = 0 }
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define void @foo(i32 %i) {
entry:
  br label %while.header

while.cond.loopexit3:
  br label %while.header

while.header:
  br label %switchbb

switchbb:
  switch i32 %i, label %unreachableA [
    i32 1, label %for.body121
    i32 2, label %unreachableA
    i32 3, label %unreachableB
  ]

unreachableA:
  unreachable

for.body121:
  %indvar = phi i32 [ 0, %switchbb ], [ %indvar.next, %for.body121 ]
  %ptr = getelementptr [1536 x float], [1536 x float]* @A, i64 0, i32 %indvar
  store float undef, float* %ptr
  %indvar.next = add nsw i32 %indvar, 1
  br i1 false, label %for.body121, label %unreachableB

unreachableB:
  unreachable
}
