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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.domain.TransactionHistory;
import com.trunks.springbootbankinc.domain.TransactionType;
import com.trunks.springbootbankinc.dto.CardAnulationTransanctioDTO;
import com.trunks.springbootbankinc.dto.CardPurchaseDTO;
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
        		.transactionAmmount(cardPurchaseDTO.getPrice())
        		.enroll(true)
        		.block(false)
        		.build();
        
        card.setAccountBalance(balance);
        card.setTransactionType(TransactionType.PURCHASETRANSACTION);
        card.setEnroll(true);
        card.setBlock(false);
        card.addTrx(transactionHistory);
        
        transactionHistory.setCard(card);
        //
      

        
       
        //update card
        Card cardResult = cardService.save(card);
        
        log.debug("Card updated: " + cardResult.getCreationDate());
    	
        TransactionInfoDTO  trxDTOResponse = TransactionInfoDTO.builder()
       		.date(transactionHistory.getDate())
		 	.cardNumber(card.getNumber())
		 	.transactionNumber(transactionHistory.getTransactionNumber())
		 	.transactionAmmount(transactionHistory.getTransactionAmmount())
		 	.accountBalance(card.getAccountBalance())
		 	.transactionType(transactionHistory.getTransactionType())
		 	.build();
    	
        return ResponseEntity.created(new URI("/card/enroll/" + trxDTOResponse.getCardNumber()))
                .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", trxDTOResponse.getCardNumber()))
                .body(trxDTOResponse);
    }
    
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
         
         //
         Optional<TransactionHistory> optionaltrxHistory = card.getTransactionHistories().stream()
             	.filter(v -> v.getTransactionNumber()!=null)
             	.filter(v -> v.getTransactionNumber().equals(cardAnulationTransanctioDTO.getTransactionId()))
             	.findAny();
         TransactionHistory trxHistory = optionaltrxHistory.orElseThrow(() -> new BadRequestAlertException("The transaction id don't exists", "TransactionHistory", "trxIdNull"));
         
         Float balance = trxHistory.getAccountBalance() + trxHistory.getTransactionAmmount();
         

         //The transaction to be canceled must not be older than 24 hours.
         Date currentDate = trxHistory.getDate();
         long longCurrentDate = currentDate.getTime();
         long longFutureDate = longCurrentDate + (24 * 60 * 60 * 1000); // 24 hours in milliseconds
         Date futureDate = new Date(longFutureDate); // Current Date + 24 hours
         
         if(!currentDate.before(futureDate)) {
        	 throw new BadRequestAlertException("The transaction to be canceled is greater than 24 hours.", "Card", "dateError");
         }
         
         //Do not reverse a reversed transaction.
         if(trxHistory.getTransactionType().equals(TransactionType.REVERSEDTRANSACTION)) {
        	 throw new BadRequestAlertException("Do not reverse a reversed transaction.", "Card", "transactionError");
         }
         
         //
         TransactionHistory transactionHistory = TransactionHistory.builder()
         	    .date(new Date())
         		.transactionType(TransactionType.ANULATIONTRANSACTION)
         		.accountBalance(balance)
         		.transactionAmmount(trxHistory.getTransactionAmmount())
         		.enroll(true)
         		.block(false)
         		.build();
         
         card.setAccountBalance(balance);
         card.setTransactionType(TransactionType.ANULATIONTRANSACTION);
         card.setEnroll(true);
         card.setBlock(false);
         card.addTrx(transactionHistory);
         
         trxHistory.setTransactionType(TransactionType.REVERSEDTRANSACTION);
         
         transactionHistory.setCard(card);
         //         
         
         
         
         //
         TransactionInfoDTO  trxDTOResponse = TransactionInfoDTO.builder()
        		.date(trxHistory.getDate())
		 		.cardNumber(card.getNumber())
		 		.transactionNumber(trxHistory.getTransactionNumber())
		 		.transactionAmmount(trxHistory.getTransactionAmmount())
		 		.accountBalance(card.getAccountBalance())
		 		.transactionType(trxHistory.getTransactionType())
		 		.build();
		 //
         
         //update card
         Card cardResult = cardService.save(card);
    	 
         return ResponseEntity.created(new URI("/card/enroll/" + cardResult.getNumber()))
                 .headers(HeaderUtil.createEntityCreationAlert(applicationName, true, "Card", cardResult.getNumber()))
                 .body(trxDTOResponse);
    }

}
