; RUN: opt %loadPolly -polly-vectorizer=polly -polly-ast -analyze < %s | FileCheck %s
;
; CHECK: if (1 && 0 == ((p >= 1 && N >= p + 1 && p <= 4) || (p <= -1 && N + p >= 1 && p >= -4)))
;
; CHECK:     #pragma minimal dependence distance: max(-p, p)
; CHECK:     #pragma dep-free iterations: 4
; CHECK:     for (int c0 = 0; c0 < N; c0 += 1)
; CHECK:       Stmt_for_body(c0);
;
; CHECK: :: isl ast :: g :: %for.cond---%for.end
;
; CHECK: if (1 && 0 == ((p >= 1 && p <= 4) || (p <= -1 && p >= -4)))
;
; CHECK:     #pragma minimal dependence distance: max(-p, p)
; CHECK:     #pragma dep-free iterations: 4
; CHECK:     for (int c0 = 0; c0 <= 1023; c0 += 1)
; CHECK:       Stmt_for_body(c0);
;
;    void f(int *A, int N, int p) {
;      for (int i = 0; i < N; i++) {
;        A[i] = A[i + p];
;      }
;    }
;    void g(int *A, int N, int p) {
;      for (int i = 0; i < 1024; i++) {
;        A[i] = A[i + p];
;      }
;    }
;
source_filename = "ptest/min-dep-dis.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @f(i32* %A, i32 %N, i32 %p) {
entry:
  %tmp = sext i32 %N to i64
  %tmp2 = sext i32 %p to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.cond
  br label %for.end

for.body:                                         ; preds = %for.cond
  %tmp3 = add nsw i64 %indvars.iv, %tmp2
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %tmp3
  %tmp4 = load i32, i32* %arrayidx, align 4, !tbaa !2
  %arrayidx2 = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  store i32 %tmp4, i32* %arrayidx2, align 4, !tbaa !2
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond.cleanup
  ret void
}

declare void @llvm.lifetime.start.p0i8(i64, i8* nocapture)

declare void @llvm.lifetime.end.p0i8(i64, i8* nocapture)

define void @g(i32* %A, i32 %N, i32 %p) {
entry:
  %tmp = sext i32 %p to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %exitcond = icmp ne i64 %indvars.iv, 1024
  br i1 %exitcond, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.cond
  br label %for.end

for.body:                                         ; preds = %for.cond
  %tmp2 = add nsw i64 %indvars.iv, %tmp
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %tmp2
  %tmp3 = load i32, i32* %arrayidx, align 4, !tbaa !2
  %arrayidx2 = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  store i32 %tmp3, i32* %arrayidx2, align 4, !tbaa !2
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond.cleanup
  ret void
}


!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0 (http://llvm.org/git/clang.git d65d042cb138b1c906acae5d6c3409bf215f6a24) (http://llvm.org/git/llvm.git 794ca809cb9a1cc26e1810571a043d97b9ba7f03)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"int", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
