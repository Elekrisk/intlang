; ModuleID = 'main'
source_filename = "main"

%VarStorage = type { i64, %BackingStore* }
%BackingStore = type { i64, %Value }
%Value = type { i8, [4 x i64] }

@variable_storage = global %VarStorage zeroinitializer

declare i8* @realloc(i8*, i64)

declare i8* @memset(i8*, i32, i64)

define void @expand_variable_storage() {
entry:
  %var-storage = load %VarStorage, %VarStorage* @variable_storage, align 8
  %old-size = extractvalue %VarStorage %var-storage, 0
  %new-size = mul i64 %old-size, 2
  %byte-size = mul i64 %new-size, ptrtoint (%BackingStore* getelementptr (%BackingStore, %BackingStore* null, i32 1) to i64)
  %old-ptr = extractvalue %VarStorage %var-storage, 1
  %ptr-casted-to-void = bitcast %BackingStore* %old-ptr to i8*
  %new_ptr = call i8* @realloc(i8* %ptr-casted-to-void, i64 %byte-size)
  %ptr-casted-to-backing-store-ptr-ptr = bitcast i8* %new_ptr to %BackingStore*
  %half-ptr = getelementptr %BackingStore, %BackingStore* %ptr-casted-to-backing-store-ptr-ptr, i64 %old-size
  %half-void-ptr = bitcast %BackingStore* %half-ptr to i8*
  %old-byte-size = mul i64 %old-size, ptrtoint (%BackingStore* getelementptr (%BackingStore, %BackingStore* null, i32 1) to i64)
  %0 = call i8* @memset(i8* %half-void-ptr, i32 0, i64 %old-byte-size)
  %var-storage-with-new-size = insertvalue %VarStorage %var-storage, i64 %new-size, 0
  %var-storage-with-new-ptr = insertvalue %VarStorage %var-storage-with-new-size, %BackingStore* %ptr-casted-to-backing-store-ptr-ptr, 1
  store %VarStorage %var-storage-with-new-ptr, %VarStorage* @variable_storage, align 8
  ret void
}

define void @initialize_variable_storage() {
entry:
  %var-storage = load %VarStorage, %VarStorage* @variable_storage, align 8
  %new_ptr = call i8* @realloc(i8* null, i64 mul (i64 ptrtoint (%BackingStore* getelementptr (%BackingStore, %BackingStore* null, i32 1) to i64), i64 16))
  %0 = call i8* @memset(i8* %new_ptr, i32 0, i64 mul (i64 ptrtoint (%BackingStore* getelementptr (%BackingStore, %BackingStore* null, i32 1) to i64), i64 16))
  %ptr-casted-to-backing-store-ptr-ptr = bitcast i8* %new_ptr to %BackingStore*
  %var-storage-with-new-size = insertvalue %VarStorage %var-storage, i64 16, 0
  %var-storage-with-new-ptr = insertvalue %VarStorage %var-storage-with-new-size, %BackingStore* %ptr-casted-to-backing-store-ptr-ptr, 1
  store %VarStorage %var-storage-with-new-ptr, %VarStorage* @variable_storage, align 8
  ret void
}

define %BackingStore* @allocate_backing_store() {
entry:
  %var-storage = load %VarStorage, %VarStorage* @variable_storage, align 8
  %size = extractvalue %VarStorage %var-storage, 0
  %backing-list = extractvalue %VarStorage %var-storage, 1
  br label %loop

loop:                                             ; preds = %loop-continued, %entry
  %loop_var = phi i64 [ 0, %entry ], [ %incd-loop-var, %loop-continued ]
  %is-at-end-of-list = icmp uge i64 %loop_var, %size
  br i1 %is-at-end-of-list, label %no-unallocated-backing-store, label %loop-continued

loop-continued:                                   ; preds = %loop
  %backing-store-ptr = getelementptr %BackingStore, %BackingStore* %backing-list, i64 %loop_var
  %backing-store = load %BackingStore, %BackingStore* %backing-store-ptr, align 4
  %backing-store-count = extractvalue %BackingStore %backing-store, 0
  %is-unallocated = icmp eq i64 %backing-store-count, 0
  %incd-loop-var = add i64 %loop_var, 1
  br i1 %is-unallocated, label %unallocated-backing-store-found, label %loop

no-unallocated-backing-store:                     ; preds = %loop
  call void @expand_variable_storage()
  br label %unallocated-backing-store-found

unallocated-backing-store-found:                  ; preds = %no-unallocated-backing-store, %loop-continued
  %backing-store-ptr1 = getelementptr %BackingStore, %BackingStore* %backing-list, i64 %loop_var
  ret %BackingStore* %backing-store-ptr1
}
