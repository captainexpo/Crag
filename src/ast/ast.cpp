#include "ast.h"

uint64_t getLitValue(const std::shared_ptr<Literal> &lit) {
    // bitcast all literals to uint64_t for simplicity
    if (auto i8 = std::dynamic_pointer_cast<U8>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto i32 = std::dynamic_pointer_cast<U32>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto i64 = std::dynamic_pointer_cast<U64>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto u16 = std::dynamic_pointer_cast<U16>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto usize = std::dynamic_pointer_cast<USize>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto i32 = std::dynamic_pointer_cast<I32>(lit->lit_type)) {
        return std::get<int64_t>(lit->value);
    }
    if (auto i64 = std::dynamic_pointer_cast<I64>(lit->lit_type)) {
        return std::get<int64_t>(lit->value);
    }
    if (auto i16 = std::dynamic_pointer_cast<I16>(lit->lit_type)) {
        return std::get<int64_t>(lit->value);
    }
    if (auto i8 = std::dynamic_pointer_cast<I8>(lit->lit_type)) {
        return std::get<int64_t>(lit->value);
    }
    if (auto f32 = std::dynamic_pointer_cast<F32>(lit->lit_type)) {
        return bitcast<double, uint64_t>(std::get<double>(lit->value));
    }
    if (auto f64 = std::dynamic_pointer_cast<F64>(lit->lit_type)) {
        return bitcast<double, uint64_t>(std::get<double>(lit->value));
    }
    if (auto str = std::dynamic_pointer_cast<PointerType>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto boolean = std::dynamic_pointer_cast<Boolean>(lit->lit_type)) {
        return std::get<bool>(lit->value) ? 1 : 0;
    }
    throw std::runtime_error("Unsupported literal type for getLitValue: " + lit->lit_type->str());
}

void setLitVal(std::shared_ptr<Literal> lit, uint64_t raw_val) {
    if (auto i8 = std::dynamic_pointer_cast<U8>(lit->lit_type)) {
        lit->value = static_cast<uint64_t>(raw_val);
        return;
    }
    if (auto i32 = std::dynamic_pointer_cast<U32>(lit->lit_type)) {
        lit->value = static_cast<uint64_t>(raw_val);
        return;
    }
    if (auto i64 = std::dynamic_pointer_cast<U64>(lit->lit_type)) {
        lit->value = static_cast<uint64_t>(raw_val);
        return;
    }
    if (auto usize = std::dynamic_pointer_cast<USize>(lit->lit_type)) {
        lit->value = static_cast<uint64_t>(raw_val);
        return;
    }
    if (auto u16 = std::dynamic_pointer_cast<U16>(lit->lit_type)) {
        lit->value = static_cast<uint64_t>(raw_val);
        return;
    }
    if (auto i32 = std::dynamic_pointer_cast<I32>(lit->lit_type)) {
        lit->value = static_cast<int64_t>(raw_val);
        return;
    }
    if (auto i64 = std::dynamic_pointer_cast<I64>(lit->lit_type)) {
        lit->value = static_cast<int64_t>(raw_val);
        return;
    }
    if (auto i16 = std::dynamic_pointer_cast<I16>(lit->lit_type)) {
        lit->value = static_cast<int64_t>(raw_val);
        return;
    }
    if (auto i8 = std::dynamic_pointer_cast<I8>(lit->lit_type)) {
        lit->value = static_cast<int64_t>(raw_val);
        return;
    }
    if (auto f32 = std::dynamic_pointer_cast<F32>(lit->lit_type)) {
        lit->value = bitcast<uint64_t, double>(raw_val);
        return;
    }
    if (auto f64 = std::dynamic_pointer_cast<F64>(lit->lit_type)) {
        lit->value = bitcast<uint64_t, double>(raw_val);
        return;
    }
    if (auto str = std::dynamic_pointer_cast<PointerType>(lit->lit_type)) {
        lit->value = raw_val;
        return;
    }
    if (auto boolean = std::dynamic_pointer_cast<Boolean>(lit->lit_type)) {
        lit->value = (raw_val != 0);
        return;
    }
    throw std::runtime_error("Unsupported literal type for setLitVal: " + lit->lit_type->str());
}

// Natural (non-packed) alignment of `type`, matching what LLVM's
// StructType::setBody(..., /*packed=*/false) will actually use. Needed by
// getTypeSize() so that struct sizes include the same inter-field and
// trailing padding LLVM inserts -- without this, sizeof() on a struct that
// mixes field sizes (e.g. a 4-byte enum next to 8-byte pointers) silently
// undercounts the real in-memory size, and every malloc(sizeof(T)) for
// such a type under-allocates.
int getTypeAlign(const std::shared_ptr<Type> &type) {
    switch (type->kind()) {
        case TypeKind::I8:
        case TypeKind::U8:
        case TypeKind::Bool:
            return 1;
        case TypeKind::I16:
        case TypeKind::U16:
            return 2;
        case TypeKind::I32:
        case TypeKind::U32:
        case TypeKind::F32:
            return 4;
        case TypeKind::Enum:
            return getTypeAlign(std::dynamic_pointer_cast<EnumType>(type)->base_type);
        case TypeKind::U64:
        case TypeKind::I64:
        case TypeKind::F64:
        case TypeKind::Pointer:
        case TypeKind::Function:
        case TypeKind::Str:
            return 8;
        case TypeKind::USize:
        case TypeKind::ISize:
            return sizeof(size_t);
        case TypeKind::Struct: {
            auto st = std::dynamic_pointer_cast<StructType>(type);
            int max_align = 1;
            for (const auto &field : st->fields) {
                max_align = std::max(max_align, getTypeAlign(field.second));
            }
            return max_align;
        }
        case TypeKind::Union: {
            auto ut = std::dynamic_pointer_cast<UnionType>(type);
            int max_align = 1;
            for (const auto &field : ut->fields) {
                max_align = std::max(max_align, getTypeAlign(field.second));
            }
            return max_align;
        }
        case TypeKind::Array: {
            auto at = std::dynamic_pointer_cast<ArrayType>(type);
            return getTypeAlign(at->element_type);
        }
        case TypeKind::ErrorUnion: {
            auto eut = std::dynamic_pointer_cast<ErrorUnionType>(type);
            return std::max(getTypeAlign(eut->valueType), getTypeAlign(eut->errorType));
        }
        case TypeKind::Void:
            return 1;
        default:
            break;
    }

    throw std::runtime_error("Unsupported type for getTypeAlign: " + type->str());
}

static inline int alignUp(int offset, int align) {
    if (align <= 1)
        return offset;
    return ((offset + align - 1) / align) * align;
}

int getTypeSize(const std::shared_ptr<Type> &type) {
    switch (type->kind()) {
        case TypeKind::I8:
        case TypeKind::U8:
            return 1;
        case TypeKind::U16:
        case TypeKind::I16:
            return 2;
        case TypeKind::U32:
        case TypeKind::I32:
        case TypeKind::F32:
            return 4;
        case TypeKind::U64:
        case TypeKind::I64:
        case TypeKind::F64:
            return 8;
        case TypeKind::ISize:
        case TypeKind::USize:
            return sizeof(size_t);
        case TypeKind::Pointer:
            return sizeof(void *);
        case TypeKind::Bool:
            return 1;
        case TypeKind::Str:
            // Matches getLLVMType's StringType lowering: { ptr, i64 }.
            return sizeof(void *) + 8;
        case TypeKind::Struct: {
            auto st = std::dynamic_pointer_cast<StructType>(type);
            int offset = 0;
            int max_align = 1;
            for (const auto &field : st->fields) {
                int falign = getTypeAlign(field.second);
                offset = alignUp(offset, falign);
                offset += getTypeSize(field.second);
                max_align = std::max(max_align, falign);
            }
            return alignUp(offset, max_align);
        }
        case TypeKind::Union: {
            auto ut = std::dynamic_pointer_cast<UnionType>(type);
            int max_size = 0;
            int max_align = 1;
            for (const auto &field : ut->fields) {
                max_size = std::max(max_size, getTypeSize(field.second));
                max_align = std::max(max_align, getTypeAlign(field.second));
            }
            return alignUp(max_size, max_align);
        }
        case TypeKind::Array: {
            auto at = std::dynamic_pointer_cast<ArrayType>(type);
            auto size_lit = std::dynamic_pointer_cast<Literal>(at->length_expr);
            if (!size_lit) {
                throw std::runtime_error("Array size is not a literal for getTypeSize: " + type->str());
            }
            uint64_t array_size = getLitValue(size_lit);
            return getTypeSize(at->element_type) * array_size;
        }
        case TypeKind::Enum: {
            return getTypeSize(std::dynamic_pointer_cast<EnumType>(type)->base_type);
        }
        case TypeKind::ErrorUnion: {
            auto eut = std::dynamic_pointer_cast<ErrorUnionType>(type);
            int val_size = getTypeSize(eut->valueType);
            int err_size = getTypeSize(eut->errorType);
            return std::max(val_size, err_size);
        }
        case TypeKind::Function:
            return sizeof(void *);
        case TypeKind::Void:
            return 0;
        default:
            break;
    }

    throw std::runtime_error("Unsupported type for getTypeSize: " + type->str());
}

bool typeContainsGeneric(const std::shared_ptr<Type> &t) {
    if (auto gt = std::dynamic_pointer_cast<GenericType>(t)) {
        return true;
    }
    if (auto st = std::dynamic_pointer_cast<StructType>(t)) {
        for (const auto &field : st->fields) {
            if (typeContainsGeneric(field.second)) {
                return true;
            }
        }
    }
    if (auto at = std::dynamic_pointer_cast<ArrayType>(t)) {
        return typeContainsGeneric(at->element_type);
    }
    if (auto et = std::dynamic_pointer_cast<EnumType>(t)) {
        return typeContainsGeneric(et->base_type);
    }
    if (auto eut = std::dynamic_pointer_cast<ErrorUnionType>(t)) {
        return typeContainsGeneric(eut->valueType) || typeContainsGeneric(eut->errorType);
    }
    if (auto ft = std::dynamic_pointer_cast<FunctionType>(t)) {
        for (const auto &param : ft->params) {
            if (typeContainsGeneric(param)) {
                return true;
            }
        }
        return typeContainsGeneric(ft->ret);
    }
    return false;
}

