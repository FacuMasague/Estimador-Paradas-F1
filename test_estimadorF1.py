"""Pruebas de cálculo y convenciones; no evalúan capacidad predictiva."""

import json
from pathlib import Path
import unittest

from estimadorF1chat import edades_stint, tiempo_stint, tiempo_vuelta


class PruebasModelo(unittest.TestCase):
    def test_casos_compartibles_con_r(self):
        datos = json.loads(
            Path(__file__).with_name("casos_sinteticos.json").read_text()
        )
        for caso in datos["casos"]:
            with self.subTest(caso=caso["nombre"]):
                edades = edades_stint(caso["edad_inicial"], caso["vueltas"])
                self.assertEqual(edades, caso["edades_esperadas"])
                for edad, esperado in zip(edades, caso["tiempos_esperados_s"], strict=True):
                    self.assertAlmostEqual(
                        tiempo_vuelta(caso["base_s"], caso["pendiente_s_por_vuelta"], edad),
                        esperado, delta=datos["tolerancia_absoluta_s"],
                    )
                self.assertAlmostEqual(
                    tiempo_stint(caso["base_s"], caso["pendiente_s_por_vuelta"],
                                 caso["edad_inicial"], caso["vueltas"]),
                    caso["total_esperado_s"], delta=datos["tolerancia_absoluta_s"],
                )

    def test_excluir_una_vuelta_no_rejuvenece_el_juego(self):
        edades = edades_stint(0, 4)
        # Se excluye del ajuste la segunda vuelta, conservando su uso físico.
        self.assertEqual([edad for i, edad in enumerate(edades) if i != 1], [1, 3, 4])

    def test_no_admite_edades_o_longitudes_invalidas(self):
        for edad, vueltas in [(-1, 3), (0, 0), (0, -1), (0, 2.5), (True, 2)]:
            with self.subTest(edad=edad, vueltas=vueltas):
                with self.assertRaises(ValueError):
                    edades_stint(edad, vueltas)

    def test_no_admite_tiempos_invalidos(self):
        for base, pendiente, edad in [(float("nan"), 0.1, 1), (90, float("inf"), 1),
                                      (0, 0.1, 1), (90, -100, 1), (90, 0.1, 0),
                                      (True, 0.1, 1), (90, 0.1, 1.5)]:
            with self.subTest(base=base, pendiente=pendiente, edad=edad):
                with self.assertRaises(ValueError):
                    tiempo_vuelta(base, pendiente, edad)


if __name__ == "__main__":
    unittest.main()
