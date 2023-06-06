package com.trunks.springbootbankinc.dto;

import com.trunks.springbootbankinc.domain.Card;

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
public class CardTrxInfoWrapperDTO {

	private Card card;
	private TransactionInfoDTO transactionInfoDTO;
}
