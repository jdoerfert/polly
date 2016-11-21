; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
;    int Q[2];
;    int ZERO;
;    int ONE;
;
;    void system(void);
;
;    int h() {
;      if (ZERO)
;        system();
;
;      return Q[1];
;    }
;
;    int g() {
;      if (ONE != 1)
;        system();
;      return h() + Q[0] + Q[h()];
;    }
;
;    void f(int *A) {
;      for (int i = 0; i < 1000; i++)
;        for (int j = 0; j < 1000; j++)
;          A[i + j + ZERO] = g();
;    }
;
source_filename = "rec_inv_load.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@ZERO = common global i32 0, align 4
@Q = common global [2 x i32] zeroinitializer, align 4
@ONE = common global i32 0, align 4

define i32 @h() {
entry:
  %ZEROinH = load i32, i32* @ZERO, align 4
  %tobool = icmp eq i32 %ZEROinH, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %entry
  call void @system() #2
  br label %if.end

if.end:                                           ; preds = %entry, %if.then
  %Q1inH = load i32, i32* getelementptr inbounds ([2 x i32], [2 x i32]* @Q, i64 0, i64 1), align 4
  ret i32 %Q1inH
}

declare void @system() #1

define i32 @g() {
entry:
  %ONEinG = load i32, i32* @ONE, align 4
  %cmp = icmp eq i32 %ONEinG, 1
  br i1 %cmp, label %if.end, label %if.then

if.then:                                          ; preds = %entry
  call void @system() #2
  br label %if.end

if.end:                                           ; preds = %entry, %if.then
  %call = call i32 @h()
  %Q0inG = load i32, i32* getelementptr inbounds ([2 x i32], [2 x i32]* @Q, i64 0, i64 0), align 4
  %add = add nsw i32 %call, %Q0inG
  %call1 = call i32 @h()
  %idxprom = sext i32 %call1 to i64
  %arrayidx = getelementptr inbounds [2 x i32], [2 x i32]* @Q, i64 0, i64 %idxprom
  %QhinG = load i32, i32* %arrayidx, align 4
  %add2 = add nsw i32 %add, %QhinG
  ret i32 %add2
}

define void @f(i32* %A) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc5, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc6, %for.inc5 ]
  %exitcond1 = icmp ne i32 %i.0, 1000
  br i1 %exitcond1, label %for.body, label %for.end7

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %j.0 = phi i32 [ 0, %for.body ], [ %inc, %for.inc ]
  %exitcond = icmp ne i32 %j.0, 1000
  br i1 %exitcond, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  %call = call i32 @g()
  %add = add nuw nsw i32 %i.0, %j.0
  %ZEROinF = load i32, i32* @ZERO, align 4
  %add4 = add nsw i32 %add, %ZEROinF
  %idxprom = sext i32 %add4 to i64
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %idxprom
  store i32 %call, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %inc = add nuw nsw i32 %j.0, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc5

for.inc5:                                         ; preds = %for.end
  %inc6 = add nuw nsw i32 %i.0, 1
  br label %for.cond

for.end7:                                         ; preds = %for.cond
  ret void
}
