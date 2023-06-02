package com.trunks.springbootbankinc.domain;

public enum DocumentType {

    CEDULACIUDADANIA("CC"),
    CEDULAEXTRANJERIA("CE");

    private String code;

    private DocumentType(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}