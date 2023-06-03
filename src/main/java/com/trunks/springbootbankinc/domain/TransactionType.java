package com.trunks.springbootbankinc.domain;

public enum TransactionType {

    NUMBERGENERATION("NUMBERGENERATION"),
    CARDACTIVATION("CARDACTIVATION"),
    CARDLOCK("CARDLOCK"),
	PURCHASETRANSACTION("PURCHASETRANSACTION");

    private String code;

    private TransactionType(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}
