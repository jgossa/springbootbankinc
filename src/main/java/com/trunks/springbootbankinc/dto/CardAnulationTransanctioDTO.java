package com.trunks.springbootbankinc.dto;

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
public class CardAnulationTransanctioDTO {
	
	private String cardId;
	private String transactionId; 
}
