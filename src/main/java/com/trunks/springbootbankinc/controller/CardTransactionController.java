package com.trunks.springbootbankinc.controller;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.Optional;

import javax.validation.Valid;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.domain.TransactionHistory;
import com.trunks.springbootbankinc.domain.TransactionType;
import com.trunks.springbootbankinc.dto.CardPurchaseDTO;
import com.trunks.springbootbankinc.exception.BadRequestAlertException;
import com.trunks.springbootbankinc.service.CardService;

import io.github.jhipster.web.util.HeaderUtil;
import lombok.extern.log4j.Log4j2;

@Log4j2
@RestController
@RequestMapping("/transaction")
public class CardTransactionController {
	
	private final CardService cardService;
	
    @Value("${spring.application.name}")
    private String applicationName;
	
	public CardTransactionController(CardService cardService) {
		this.cardService = cardService;
	}
	
    @PostMapping("/purchase")
    public ResponseEntity<Card> purchaseCard(@Valid @RequestBody CardPurchaseDTO cardPurchaseDTO) throws URISyntaxException {
    	
        log.debug("REST request to reaload a card : {}", cardPurchaseDTO.getCardId());
        
        //Search the card
        Optional<Card> optionalCard = cardService.findByCardNumber(cardPurchaseDTO.getCardId());
        Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card didn´t exist", "Card", "cardIdNull"));
        
        //Check if the card is enroll
        if(Boolean.FALSE.equals(card.getEnroll())) {
        	throw new BadRequestAlertException("The card has not been activated yet", "Card", "inactiveCard");
        }
        
        //Check if the card is blocked
        if(Boolean.TRUE.equals(card.getBlock())) {
        	throw new BadRequestAlertException("The card is blocked", "Card", "blockedCard");
        }
        
        //Check expiration date
        if(new Date().compareTo(card.getDueDate()) >= 0) {
        	throw new BadRequestAlertException("The card is expired", "Card", "expiredCard");
        }
        
        //Check negative balance
        if(card.getAccountBalance() < 0L) {
        	throw new BadRequestAlertException("The card balance is negative", "Card", "negativeCardBalance");
        	
        }
        
        //check if the balance is adequate
        Float balance = card.getAccountBalance() - cardPurchaseDTO.getPrice();
        
        if(balance.floatValue() < 0.0) {
        	throw new BadRequestAlertException("The card balance is insufficient", "Card", "insuficientCardBalance");
        }
        
        //Generate transaction number
        String inicString = RandomStringUtils.randomNumeric(50) + cardPurchaseDTO.getCardId();
        String sha256TransactionNumber = DigestUtils.sha256Hex(inicString);
        
        
        
        //
        TransactionHistory transactionHistory = TransactionHistory.builder()
        	    .date(new Date())
        		.transactionType(TransactionType.PURCHASETRANSACTION)
        		.transactionNumber(sha256TransactionNumber)
        		.accountBalance(balance)
        		.enroll(true)
        		.block(false)
        		.build();
        
        card.setAccountBalance(balance);
        card.setTransactionType(TransactionType.PURCHASETRANSACTION);
        card.setTransactionNumber(sha256TransactionNumber);
        card.setEnroll(true);
        card.setBlock(false);
        card.addTrx(transactionHistory);
        
        transactionHistory.setCard(card);
        //
      

        
       
        //update card
        Card cardResult = cardService.save(card);
        
        log.debug("Card updated: " + cardResult.getCreationDate());
    	
    	Card cardResponse = Card.builder().number(card.getNumber()).transactionNumber(card.getTransactionNumber()).build();
    	
        return ResponseEntity.created(new URI("/card/enroll/" + cardResponse.getNumber()))
                .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", cardResponse.getNumber()))
                .body(cardResponse);
    }

}
