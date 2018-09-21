; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
; FIXME: Edit the run line and add checks!
;
; XFAIL: *
;
;    void f(long *A, long N) {
;      __builtin_assume(N > 0);
;      long i, j;
;      for (i = 0; i < N; i++)
;      S:
;        A[i * N] += i;
;      for (j = 0; j < N; j++)
;      P:
;        A[(j + N) * N] += j;
;    }
;
source_filename = "../polly/test/DependenceInfo/scev_interval_test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @f(i64* %A, i64 %N) {
entry:
  %cmp = icmp sgt i64 %N, 0
  call void @llvm.assume(i1 %cmp)
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i.0 = phi i64 [ 0, %entry ], [ %inc, %for.inc ]
  %exitcond1 = icmp ne i64 %i.0, %N
  br i1 %exitcond1, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  br label %S

S:                                                ; preds = %for.body
  %mul = mul nsw i64 %i.0, %N
  %arrayidx = getelementptr inbounds i64, i64* %A, i64 %mul
  %tmp = load i64, i64* %arrayidx, align 8, !tbaa !2
  %add = add nsw i64 %tmp, %i.0
  store i64 %add, i64* %arrayidx, align 8, !tbaa !2
  br label %for.inc

for.inc:                                          ; preds = %S
  %inc = add nuw nsw i64 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc9, %for.end
  %j.0 = phi i64 [ 0, %for.end ], [ %inc10, %for.inc9 ]
  %exitcond = icmp ne i64 %j.0, %N
  br i1 %exitcond, label %for.body4, label %for.end11

for.body4:                                        ; preds = %for.cond2
  br label %P

P:                                                ; preds = %for.body4
  %add5 = add nuw nsw i64 %j.0, %N
  %mul6 = mul nsw i64 %add5, %N
  %arrayidx7 = getelementptr inbounds i64, i64* %A, i64 %mul6
  %tmp2 = load i64, i64* %arrayidx7, align 8, !tbaa !2
  %add8 = add nsw i64 %tmp2, %j.0
  store i64 %add8, i64* %arrayidx7, align 8, !tbaa !2
  br label %for.inc9

for.inc9:                                         ; preds = %P
  %inc10 = add nuw nsw i64 %j.0, 1
  br label %for.cond2

for.end11:                                        ; preds = %for.cond2
  ret void
}

declare void @llvm.assume(i1)

declare void @llvm.lifetime.start.p0i8(i64, i8* nocapture)

declare void @llvm.lifetime.end.p0i8(i64, i8* nocapture)


!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0 (http://llvm.org/git/clang.git d20f95dfb11ea73212982a1d321eb81933d42d6a) (http://llvm.org/git/llvm.git 334d072b0ce90862614838fc912bd5b41c5fb786)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"long", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
