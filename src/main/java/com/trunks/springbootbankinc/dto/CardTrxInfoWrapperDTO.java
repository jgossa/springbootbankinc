package com.trunks.springbootbankinc.dto;

import com.trunks.springbootbankinc.domain.Card;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class CardTrxInfoWrapperDTO {

	private Card card;
	private TransactionInfoDTO transactionInfoDTO;
}
