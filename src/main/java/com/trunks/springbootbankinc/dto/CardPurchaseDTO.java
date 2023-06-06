package com.trunks.springbootbankinc.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Setter
@Getter
public class CardPurchaseDTO {
	
	private String cardId;
	
	private Float price;

}
