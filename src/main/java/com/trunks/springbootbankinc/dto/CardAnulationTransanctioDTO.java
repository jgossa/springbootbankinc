package com.trunks.springbootbankinc.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Setter
@Getter
public class CardAnulationTransanctioDTO {
	
	private String cardId;
	private String transactionId; 
}
