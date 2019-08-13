/**
 * Made by Entioni
 */

export enum HttpStatus {
    BadRequest = 400,
    Unauthorized = 401,
    PaymentRequired = 402,
    Forbidden = 403,
    NotFound = 404,
    Conflict = 409,
    Gone = 410,
    PayloadTooLarge = 413,
    UnprocessableEntity = 422,
    TooManyRequests = 429,
    InternalServerError = 500
}

export const HTTP_REASONS: Record<HttpStatus, string> = {
    '400': 'Bad request',
    '401': 'Unauthorized',
    '402': 'Payment required',
    '403': 'Forbidden',
    '404': 'Not found',
    '409': 'Conflict',
    '410': 'Gone',
    '413': 'Payload too large',
    '422': 'Validation error',
    '429': 'Too many requests',
    '500': 'Internal server error'
}

export const httpSymbol = Symbol('http')

export class HttpError extends Error {
    // for some reason instanceof stopped working at some point
    public [httpSymbol] = true

    public constructor(
        public status: HttpStatus = HttpStatus.InternalServerError,
        public reason?: string
    ) {
        super()
        this.reason = reason || HTTP_REASONS[status]
    }

    public toString() {
        return `HttpError: ${this.status} - ${this.reason}`
    }
}
