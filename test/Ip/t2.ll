; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
; FIXME: Edit the run line and add checks!
;
; XFAIL: *
;
;
;    void f(int *A, int i, int k) {
;      for (int j = 0; j < k; j++)
;        A[j + i] = i;
;    }
;
;    int *G;
;    void loop(int N) {
;      for (int i = 0; i < 200; i++) {
;        for (int j = 0; j < 100; j++)
;          f(G, i, j);
;      }
;    }
;
source_filename = "t2.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@G = common global i32* null, align 8

define void @f(i32* %A, i32 %i, i32 %k) {
entry:
  %tmp = sext i32 %k to i64
  %tmp2 = sext i32 %i to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %tmp3 = add nsw i64 %indvars.iv, %tmp2
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %tmp3
  store i32 %i, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}

define void @loop(i32 %N) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc4, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc5, %for.inc4 ]
  %exitcond1 = icmp ne i32 %i.0, 200
  br i1 %exitcond1, label %for.body, label %for.end6

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %j.0 = phi i32 [ 0, %for.body ], [ %inc, %for.inc ]
  %exitcond = icmp ne i32 %j.0, 100
  br i1 %exitcond, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  %tmp = load i32*, i32** @G, align 8
  call void @f(i32* %tmp, i32 %i.0, i32 %j.0)
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %inc = add nuw nsw i32 %j.0, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc4

for.inc4:                                         ; preds = %for.end
  %inc5 = add nuw nsw i32 %i.0, 1
  br label %for.cond

for.end6:                                         ; preds = %for.cond
  ret void
}
