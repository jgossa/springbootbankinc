package com.trunks.springbootbankinc.controller;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.Optional;

import javax.validation.Valid;

import org.apache.commons.lang3.RandomStringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.domain.Customer;
import com.trunks.springbootbankinc.domain.TransactionHistory;
import com.trunks.springbootbankinc.domain.TransactionType;
import com.trunks.springbootbankinc.dto.CardEnrollDTO;
import com.trunks.springbootbankinc.dto.CardReloadDTO;
import com.trunks.springbootbankinc.exception.BadRequestAlertException;
import com.trunks.springbootbankinc.service.CardService;

import io.github.jhipster.web.util.HeaderUtil;
import io.github.jhipster.web.util.ResponseUtil;
import lombok.extern.log4j.Log4j2;

@Log4j2
@RestController
@RequestMapping("/card")
public class CardController {
	
	private final CardService cardService;
	
    @Value("${spring.application.name}")
    private String applicationName;
	
	public CardController(CardService cardService) {
		this.cardService = cardService;
	}
	
    @GetMapping("/{productid}/{doctype}/{doc}/number")
    public ResponseEntity<String> getCardNumber(@PathVariable String productid, @PathVariable String doctype, @PathVariable String doc) {
        log.info("REST request to get MaestroContextoItem : {} {} {}", productid, doctype, doc);
        
        cardService.validateCardNumberParams(productid, doc, doctype);
        
        Customer customer = cardService.validateDbCustomer(doctype, doc);

        String idCard = productid + RandomStringUtils.randomNumeric(10);
 
        cardService.validateCardInDb(idCard);
        
        cardService.validateProductIdInCustomerCard(productid, customer);
        
        Card card = cardService.buidCard(idCard, productid, customer);
        
        Card cardResult = cardService.save(card);
        
        log.debug("Card created: " + cardResult.getCreationDate());
        
    	Optional<String> tmpOptional = Optional.of(idCard);
    	
    	return ResponseUtil.wrapOrNotFound(tmpOptional);
    }
    
    @PostMapping("/enroll")
    public ResponseEntity<Card> activateCard(@Valid @RequestBody CardEnrollDTO cardEnrollDTO) throws URISyntaxException {
    	
        log.debug("REST request to activate a card : {}", cardEnrollDTO.getCardId());
    	
        //Search the card
        Optional<Card> optionalCard = cardService.findByCardNumber(cardEnrollDTO.getCardId());
        Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card didn´t exist", "Card", "cardIdNull"));
        
        //verification of card activation
        if(Boolean.TRUE.equals(card.getEnroll())) {
        	throw new BadRequestAlertException("The card already has an activation", "Card", " already enroll");
        }
        
        
        //
        TransactionHistory transactionHistory = TransactionHistory.builder()
        	    .date(new Date())
        		.transactionType(TransactionType.CARDACTIVATION)
        		.accountBalance(Float.valueOf("0.0"))
        		.enroll(true)
        		.block(false)
        		.build();
        
        card.setTransactionType(TransactionType.CARDACTIVATION);
        card.setEnroll(true);
        card.setBlock(false);
        card.addTrx(transactionHistory);
        
        transactionHistory.setCard(card);
        //
        
        
        //update card
        Card cardResult = cardService.save(card);
        
        log.debug("Card updated: " + cardResult.getCreationDate());
    	
    	Card cardResponse = Card.builder().number(cardResult.getNumber()).build();
    	
        return ResponseEntity.created(new URI("/card/enroll/" + cardResponse.getNumber()))
                .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", cardResponse.getNumber()))
                .body(cardResponse);
    }
    
    @DeleteMapping("{cardId}")
    public ResponseEntity<Void> blockCard(@PathVariable String cardId){
    	 log.info("REST request to block a card : {}", cardId);

         //Search the card
         Optional<Card> optionalCard = cardService.findByCardNumber(cardId);
         Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card didn´t exist", "Card", "cardIdNull"));
         
         
         //
         TransactionHistory transactionHistory = TransactionHistory.builder()
         	    .date(new Date())
         		.transactionType(TransactionType.CARDLOCK)
         		.accountBalance(Float.valueOf("0.0"))
         		.enroll(true)
         		.block(true)
         		.build();
         
         card.setTransactionType(TransactionType.CARDLOCK);
         card.setEnroll(true);
         card.setBlock(true);
         card.addTrx(transactionHistory);
         
         transactionHistory.setCard(card);
         //
         
         
         //update card
         Card cardResult = cardService.save(card);
         
         log.info("Card updated: " + cardResult.getCreationDate());
    	 
    	 return ResponseEntity.noContent().headers(HeaderUtil.createEntityDeletionAlert(applicationName, true, "Card", cardId)).build(); 
    } 
    
    @PostMapping("/balance")
    public ResponseEntity<Card> reloadCard(@Valid @RequestBody CardReloadDTO cardReloadDTO) throws URISyntaxException {
    	
        log.debug("REST request to reaload a card : {}", cardReloadDTO.getCardId());
        
        //Search the card
        Optional<Card> optionalCard = cardService.findByCardNumber(cardReloadDTO.getCardId());
        Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card didn´t exist", "Card", "cardIdNull"));

        //validate if the card is enroll
        if(Boolean.FALSE.equals(card.getEnroll())) {
        	throw new BadRequestAlertException("The card has not been activated yet", "Card", "inactiveCard");
        }
        
        //validate if the card is blocked
        if(Boolean.TRUE.equals(card.getBlock())) {
        	throw new BadRequestAlertException("The card is blocked", "Card", "blockedCard");
        }
                
        Float balance = card.getAccountBalance() + Float.valueOf(cardReloadDTO.getBalance());
        
        
        //
        TransactionHistory transactionHistory = TransactionHistory.builder()
        	    .date(new Date())
        		.transactionType(TransactionType.CARDLOCK)
        		.accountBalance(balance)
        		.enroll(true)
        		.block(false)
        		.build();
        
        card.setAccountBalance(balance);
        card.setTransactionType(TransactionType.CARDLOCK);
        card.setEnroll(true);
        card.setBlock(false);
        card.addTrx(transactionHistory);
        
        transactionHistory.setCard(card);
        //

        
        
        //update card
        Card cardResult = cardService.save(card);
        
        log.debug("Card updated: " + cardResult.getCreationDate());
    	
    	Card cardResponse = Card.builder().number(cardResult.getNumber()).build();
    	
        return ResponseEntity.created(new URI("/card/enroll/" + cardResponse.getNumber()))
                .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", cardResponse.getNumber()))
                .body(cardResponse);
    }
    
    @GetMapping("/balance/{cardId}")
    public ResponseEntity<String> getCardBalance(@PathVariable String cardId) {
        log.debug("REST request to get EstructuraPrueba : {}", cardId);
        
        //Search the card
        Optional<Card> optionalCard = cardService.findByCardNumber(cardId);
        Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card didn´t exist", "Card", "cardIdNull"));
        
        Optional<String> optionalBalance = Optional.of(card.getAccountBalance().toString());
    	
    	return ResponseUtil.wrapOrNotFound(optionalBalance);
    }

}
