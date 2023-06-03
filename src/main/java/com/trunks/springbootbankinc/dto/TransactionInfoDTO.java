package com.trunks.springbootbankinc.dto;

import java.util.Date;

import com.trunks.springbootbankinc.domain.TransactionType;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
@Builder
public class TransactionInfoDTO {
	
	private Date date;
	
	private String cardNumber;
	
    private String transactionNumber;
	
	private Float accountBalance;
	
	private TransactionType transactionType;

}
