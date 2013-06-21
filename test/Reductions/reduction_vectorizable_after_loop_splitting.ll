; RUN: opt %loadPolly %defaultOpts -polly-reductions -polly-opt-isl -polly-codegen -polly-vectorizer=polly -analyze -stats < %s 2>&1 | FileCheck %s
; ModuleID = 'reduction_vectorizable_after_loop_splitting.s'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

;  int nored, sum, A[1024];
;  void f() {
;    int i = 0;
;    for (i = 0; i < 1024; i++) {
;      sum   += A[i];
;      nored  = nored + A[i] * nored;
;    }
;  }

@A = common global [1024 x i32] zeroinitializer, align 16
@sum = common global i32 0, align 4
@nored = common global i32 0, align 4

; Function Attrs: nounwind uwtable
define void @f() #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond = icmp ne i32 %lftr.wideiv, 1024
  br i1 %exitcond, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %arrayidx = getelementptr inbounds [1024 x i32]* @A, i64 0, i64 %indvars.iv
  %tmp = load i32* %arrayidx, align 4
  %tmp1 = load i32* @sum, align 4
  %add = add nsw i32 %tmp1, %tmp
  store i32 %add, i32* @sum, align 4
  br label %for.body.manual.split

for.body.manual.split:                            ; preds = %for.body
  %tmp2 = load i32* @nored, align 4
  %arrayidx2 = getelementptr inbounds [1024 x i32]* @A, i64 0, i64 %indvars.iv
  %tmp3 = load i32* %arrayidx2, align 4
  %mul = mul nsw i32 %tmp3, %tmp2
  %add3 = add nsw i32 %tmp2, %mul
  store i32 %add3, i32* @nored, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body.manual.split
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}

attributes #0 = { nounwind uwtable }

; Check for a valid reduction access (sum) and the vectorization after loop splitting
; CHECK: 1 {{.*}} Number of valid reduction accesses
; CHECK: 1 {{.*}} Number of vectorized reduction accesses
