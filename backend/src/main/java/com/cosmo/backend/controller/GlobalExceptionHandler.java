package com.cosmo.backend.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;

/**
 * Returns a consistent JSON error body for 500 and request-body errors
 * so the frontend can show a clear message instead of a generic "HTTP 500".
 */
@RestControllerAdvice
public class GlobalExceptionHandler {

    private static final Logger logger = LoggerFactory.getLogger(GlobalExceptionHandler.class);

    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<Map<String, Object>> handleBadRequest(HttpMessageNotReadableException e) {
        logger.warn("Bad request body or JSON: {}", e.getMessage());
        String message = e.getCause() != null ? e.getCause().getMessage() : e.getMessage();
        if (message != null && message.length() > 200) {
            message = message.substring(0, 200) + "...";
        }
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(Map.of(
                        "success", false,
                        "message", "Invalid request body: " + (message != null ? message : "parse error"),
                        "status", "failed"
                ));
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Map<String, Object>> handleUnexpected(Exception e) {
        logger.error("Unexpected error: ", e);
        String message = e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName();
        if (message.length() > 300) {
            message = message.substring(0, 300) + "...";
        }
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(Map.of(
                        "success", false,
                        "message", "Server error: " + message,
                        "status", "failed"
                ));
    }
}
