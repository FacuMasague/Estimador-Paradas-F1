"""Primer incremento: fórmulas de ritmo, sin descarga ni interfaz.

Todos los tiempos se expresan en segundos. No estima parámetros ni certifica
viabilidad: el llamador deberá controlar datos, extrapolación y reglamento.
"""

from math import isfinite
from numbers import Real


def _numero_finito(valor, nombre):
    if isinstance(valor, bool) or not isinstance(valor, Real) or not isfinite(valor):
        raise ValueError(f"{nombre} debe ser un número finito")


def edades_stint(edad_inicial, vueltas):
    """Edades al FINAL de cada vuelta: a_j = edad_inicial + j, j=1..vueltas.

    edad_inicial: vueltas completas de uso anteriores al stint (entero >= 0).
    vueltas: longitud del stint (entero > 0), incluyendo vueltas que luego se
    excluyan del ajuste. Limpiar tiempos no debe renumerar estas edades.
    """
    if type(edad_inicial) is not int or edad_inicial < 0:
        raise ValueError("edad_inicial debe ser un entero no negativo")
    if type(vueltas) is not int or vueltas <= 0:
        raise ValueError("vueltas debe ser un entero positivo")
    return list(range(edad_inicial + 1, edad_inicial + vueltas + 1))


def tiempo_vuelta(base_s, pendiente_s_por_vuelta, edad):
    """t(a) = b + d*a; edad al final de la vuelta, en vueltas completas >= 1.

    b: intercepto en segundos a edad cero (puede ser una referencia hipotética).
    d: cambio en segundos por vuelta adicional de edad; admite valores negativos,
    que requieren interpretación antes de extrapolar y no son desgaste físico.
    No incluye pérdida adicional por boxes ni efectos de largada.
    """
    _numero_finito(base_s, "base_s")
    _numero_finito(pendiente_s_por_vuelta, "pendiente_s_por_vuelta")
    if type(edad) is not int or edad < 1:
        raise ValueError("edad debe ser un entero positivo")
    if base_s <= 0:
        raise ValueError("base_s debe ser positivo")
    tiempo = base_s + pendiente_s_por_vuelta * edad
    if not isfinite(tiempo) or tiempo <= 0:
        raise ValueError("el tiempo proyectado debe ser finito y positivo")
    return tiempo


def tiempo_stint(base_s, pendiente_s_por_vuelta, edad_inicial, vueltas):
    """Suma tiempos normales del tramo, en segundos, sin costo de boxes.

    Equivale a n*b + d*(n*a0 + n*(n+1)/2), con n=vueltas y a0=edad_inicial.
    Se suman las vueltas explícitamente para facilitar su inspección inicial.
    """
    return sum(
        tiempo_vuelta(base_s, pendiente_s_por_vuelta, edad)
        for edad in edades_stint(edad_inicial, vueltas)
    )
