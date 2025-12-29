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
    if (auto usize = std::dynamic_pointer_cast<USize>(lit->lit_type)) {
        return std::get<uint64_t>(lit->value);
    }
    if (auto i32 = std::dynamic_pointer_cast<I32>(lit->lit_type)) {
        return std::get<int64_t>(lit->value);
    }
    if (auto i64 = std::dynamic_pointer_cast<I64>(lit->lit_type)) {
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
    if (auto i32 = std::dynamic_pointer_cast<I32>(lit->lit_type)) {
        lit->value = static_cast<int64_t>(raw_val);
        return;
    }
    if (auto i64 = std::dynamic_pointer_cast<I64>(lit->lit_type)) {
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
    throw std::runtime_error("Unsupported literal type for setLitVal: " + lit->lit_type->str());
}

int getTypeSize(const std::shared_ptr<Type> &type) {
    switch (type->kind()) {
        // case TypeKind::I8:
        case TypeKind::U8:
            return 1;
        // case TypeKind::U16:
        // case TypeKind::I16:
        //     return 2;
        case TypeKind::U32:
        case TypeKind::I32:
        case TypeKind::F32:
            return 4;
        case TypeKind::U64:
        case TypeKind::I64:
        case TypeKind::F64:
            return 8;
        // case TypeKind::ISize:
        case TypeKind::USize:
            return sizeof(size_t);
        case TypeKind::Pointer:
            return sizeof(void *);
        case TypeKind::Bool:
            return 1;
        case TypeKind::Struct: {
            auto st = std::dynamic_pointer_cast<StructType>(type);
            int total_size = 0;
            for (const auto &field : st->fields) {
                total_size += getTypeSize(field.second);
            }
            return total_size;
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
