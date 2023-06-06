package com.trunks.springbootbankinc.dto;

import java.util.Date;

import com.trunks.springbootbankinc.domain.TransactionType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionInfoDTO {
	
	private Date date;
	
	private String cardNumber;
	
    private String transactionNumber;
	
	private Float accountBalance;
	private Float transactionAmmount;
	
	private TransactionType transactionType;

}
