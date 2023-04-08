package util

/**
 * An IterationLimiter keeps count of how many times it has been incremented and
 * throws [[IterationLimitException]] if incrementing is attempted when the limit has already been reached.
 */
class IterationLimiter private (l: Int) {

  /** The amount of times this IterationLimiter can be incremented before an [[IterationLimitException]] is thrown. */
  val limit = l

  private var currentValue = 0

  /** The amount of times this IterationLimiter has been incremented. */
  def value: Int = this.currentValue

  /**
   * Increments the count of this IterationLimiter if allowed,
   * if incrementing is not allowed, throws [[IterationLimitException]].
   */
  def increment() = {
    if (this.currentValue < this.limit) {
      this.currentValue = this.currentValue + 1
    } else {
      throw new IterationLimitException
    }
  }

  /** Resets the iteration count of this IterationLimiter back to 0. */
  def reset() = {
    this.currentValue = 0
  }

  /**
   * Returns a textual representation for this IterationLimiter that contains its current value and limit.
   * For example: `Limiter(3/10)`.
   */
  override def toString: String = s"Limiter(${this.value}/${this.limit})"

}

/**
 * This companion object contains methods for creating [[IterationLimiter]]s.
 */
object IterationLimiter {
  /**
   * Creates a new [[IterationLimiter]] with limit '''limit'''.
   * @param limit the limit for the new IterationLimiter, must be greater than 0
   * @return a new new IterationLimiter
   */
  def apply(limit: Int) = {
    assert(limit > 0)
    new IterationLimiter(limit)
  }
}
