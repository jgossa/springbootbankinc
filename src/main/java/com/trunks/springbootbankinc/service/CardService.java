package com.trunks.springbootbankinc.service;

import java.util.Optional;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.domain.Customer;
import com.trunks.springbootbankinc.domain.TransactionType;
import com.trunks.springbootbankinc.dto.CardAnulationTransanctioDTO;
import com.trunks.springbootbankinc.dto.CardPurchaseDTO;
import com.trunks.springbootbankinc.dto.CardTrxInfoWrapperDTO;
import com.trunks.springbootbankinc.dto.TransactionInfoDTO;

public interface CardService {
	
	Card save(Card card);
	
	Optional<Card> findByCardNumber(String number);

	Optional<Card> findCardByTransactionNumber(String transactionId);
	
	void validateCardNumberParams(String productId, String document, String documentType);

	Customer validateDbCustomer(String documentType, String document);

	Card buidCard(String idCard, String productid, Customer customer);

	void validateProductIdInCustomerCard(String productid, Customer customer);

	void validateCardInDb(String idCard);

	TransactionInfoDTO buildTransactionDTOResponse(Card card, String transactionId);

	Card buildCardTrxHistory(Card card, TransactionType transactionType, boolean enroll, boolean block, Float balance, Float trxAmmount);

	CardTrxInfoWrapperDTO buildPurchaseTrxInfoDTO(Card card, CardPurchaseDTO cardPurchaseDTO);

	CardTrxInfoWrapperDTO buildAnulationTrxInfoDTO(Card card, CardAnulationTransanctioDTO cardAnulationTransanctioDTO);

}
