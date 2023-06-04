package com.trunks.springbootbankinc.controller;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.dto.CardAnulationTransanctioDTO;
import com.trunks.springbootbankinc.dto.CardPurchaseDTO;
import com.trunks.springbootbankinc.dto.CardTrxInfoWrapperDTO;
import com.trunks.springbootbankinc.dto.TransactionInfoDTO;
import com.trunks.springbootbankinc.exception.BadRequestAlertException;
import com.trunks.springbootbankinc.service.CardService;

import io.github.jhipster.web.util.HeaderUtil;
import io.github.jhipster.web.util.ResponseUtil;
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
    public ResponseEntity<TransactionInfoDTO> purchaseCard(@Valid @RequestBody CardPurchaseDTO cardPurchaseDTO) throws URISyntaxException {
    	
        log.debug("REST request to do a purchase: {}", cardPurchaseDTO.getCardId());
        
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
        if(card.getAccountBalance() < 0.0F) {
        	throw new BadRequestAlertException("The card balance is negative", "Card", "negativeCardBalance");
        	
        }
        
        CardTrxInfoWrapperDTO wrapper = cardService.buildPurchaseTrxInfoDTO(card, cardPurchaseDTO);
        
        //update card
        Card cardResult = cardService.save(wrapper.getCard());
        
        log.debug("Card updated: " + cardResult.getCreationDate());
    	
        return ResponseEntity.created(new URI("/transaction/purchase/" + wrapper.getTransactionInfoDTO().getCardNumber()))
                .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", wrapper.getTransactionInfoDTO().getCardNumber()))
                .body(wrapper.getTransactionInfoDTO());    }
    
    @GetMapping("/{transactionId}")
    public ResponseEntity<TransactionInfoDTO> getPurchaseTransaction(@PathVariable String transactionId) {
    	
        log.debug("REST request to get Card by transaction: {}", transactionId);
        
        //Search the card with the idTransaction
        Optional<Card> optionalCard = cardService.findCardByTransactionNumber(transactionId);        
        Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card didn´t exist", "Card", "cardIdNull"));
        
        TransactionInfoDTO trxDTOResponse = cardService.buildTransactionDTOResponse(card, transactionId);
        
        return ResponseUtil.wrapOrNotFound(Optional.ofNullable(trxDTOResponse));
    }
    
    @PostMapping("/anulation")
    public ResponseEntity<TransactionInfoDTO> anulationTransaction(@RequestBody CardAnulationTransanctioDTO cardAnulationTransanctioDTO) throws URISyntaxException {
    	
    	 log.debug("REST request to process anulation transaction : {}", cardAnulationTransanctioDTO.getCardId());
    	 
         //Search the card with the idTransaction
         Optional<Card> optionalCard = cardService.findCardByTransactionNumber(cardAnulationTransanctioDTO.getTransactionId());        
         Card card = optionalCard.orElseThrow(() -> new BadRequestAlertException("The card do not exist", "Card", "cardIdNull"));
         
         //Check if the card is enroll
         if(Boolean.FALSE.equals(card.getEnroll())) {
         	throw new BadRequestAlertException("The card has not been activated yet", "Card", "inactiveCard");
         }
         
         //Check if the card is blocked
         if(Boolean.TRUE.equals(card.getBlock())) {
         	throw new BadRequestAlertException("The card is blocked", "Card", "blockedCard");
         }
         
         CardTrxInfoWrapperDTO wrapper = cardService.buildAnulationTrxInfoDTO(card, cardAnulationTransanctioDTO);
         
         //update card
         Card cardResult = cardService.save(wrapper.getCard());

         log.debug("Card updated: " + cardResult.getCreationDate());
         
         return ResponseEntity.created(new URI("/transaction/anulation/" + wrapper.getTransactionInfoDTO().getCardNumber()))
                 .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", wrapper.getTransactionInfoDTO().getCardNumber()))
                 .body(wrapper.getTransactionInfoDTO());
    }

}
